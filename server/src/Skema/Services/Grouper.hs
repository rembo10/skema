{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Grouper service - groups files by album.
module Skema.Services.Grouper
  ( startGrouperService
  , handleMetadataReadComplete
  ) where

import Skema.Services.Types (ServiceContext(..))
import Skema.Services.EventLoop (JobMode(..), runEventService)
import Skema.Domain.Metadata (metadataRecordToMonatone)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getAllClusters, getAllTracks, getMetadataForTrack, getTrackByPath, computeClusterHash, findClusterByHash, createCluster, updateTrackCluster, updateClusterQuality, deleteEmptyClusters)
import Skema.Database.Types (ClusterRecord(..), LibraryTrackRecord(..))
import Skema.Domain.Identification (mkIdentifyConfig, partitionClusters)
import Skema.Config.Types (Config(..))
import Skema.Domain.Matching (groupFilesByRelease)
import Skema.MusicBrainz.Types (FileGroup(..))
import Control.Concurrent.Async (Async)
import Skema.Clock (getNow)
import Katip

-- | Start the grouper service.
--
-- This service listens for METADATA_READ_COMPLETE events and groups files by album.
-- It runs after metadata has been read and stored in the database.
-- Grouping runs sequentially: a regroup reassigns cluster_ids and prunes
-- empty clusters across the whole library, so two running concurrently
-- would race against each other.
-- Returns the async handle for graceful shutdown.
startGrouperService :: ServiceContext -> IO (Async ())
startGrouperService deps =
  runEventService (scLogEnv deps) (scEventBus deps) "services.grouper" Sequential $ \case
    MetadataReadComplete{..} -> Just $ handleMetadataReadComplete deps filesProcessed
    _ -> Nothing

-- | Handle metadata read complete event.
-- Groups all files in the library by album (not just changed files, since
-- grouping needs to see the full album to create proper clusters).
handleMetadataReadComplete :: ServiceContext -> Int -> IO ()
handleMetadataReadComplete ServiceContext{..} fileCount = do
  let le = scLogEnv
  let pool = scDbPool
  let bus = scEventBus
  let initialContext = ()
  let initialNamespace = "services.grouper"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr $ ("Grouping " <> show fileCount <> " files by album" :: Text)

    -- Load all tracks with metadata from database
    tracksWithMeta <- liftIO $ withConnection pool $ \conn -> do
      tracks <- getAllTracks conn
      -- For each track, get its metadata and convert to Monatone Metadata
      forM tracks $ \track -> do
        maybeMeta <- getMetadataForTrack conn (fromMaybe 0 (trackId track))
        case maybeMeta of
          Just meta -> do
            let monatoneMeta = metadataRecordToMonatone meta
            pure $ Just (trackPath track, monatoneMeta)
          Nothing -> pure Nothing

    let validTracks = catMaybes tracksWithMeta

    -- Group files by album
    let fileGroups = groupFilesByRelease validTracks

    $(logTM) InfoS $ logStr $ ("Found " <> show (length fileGroups) <> " album groups" :: Text)

    -- Create or find clusters for each group, collecting touched cluster IDs
    touchedIds <- liftIO $ withConnection pool $ \conn -> do
      forM fileGroups $ \fg -> do
        let album = fgAlbum fg
        let albumArtist = fgArtist fg
        let trackCount = length (fgFiles fg)
        let hash = computeClusterHash album albumArtist trackCount

        -- Find existing cluster or create new one
        maybeCluster <- findClusterByHash conn hash
        cid <- case maybeCluster of
          Just cluster -> pure (fromMaybe 0 (clusterId cluster))
          Nothing -> createCluster conn hash album albumArtist trackCount

        -- Get track IDs and their old cluster assignments
        trackInfos <- forM (fgFiles fg) $ \(path, _) -> do
          maybeTrack <- getTrackByPath conn path
          pure $ case maybeTrack of
            Just track -> case trackId track of
              Just tid -> Just (tid, trackClusterId track)
              Nothing -> Nothing
            Nothing -> Nothing

        let validTrackInfos = catMaybes trackInfos
        let validTrackIds = map fst validTrackInfos

        -- Assign tracks to cluster
        when (not (null validTrackIds)) $ do
          updateTrackCluster conn cid validTrackIds
          -- Compute and store cluster quality from track audio metadata
          updateClusterQuality conn cid

        -- Cluster is touched only if it's new or any track changed cluster assignment
        let isNewCluster = isNothing maybeCluster
        let anyTrackChanged = any (\(_, oldCid) -> oldCid /= Just cid) validTrackInfos
        pure $ if isNewCluster || anyTrackChanged then Just cid else Nothing

    -- Prune clusters left with no tracks. When a track's identifying metadata
    -- changes (e.g. an album artist is added) it moves to a different cluster,
    -- and the cluster it left can be orphaned while still holding its old,
    -- possibly locked, match. Those stale clusters are meaningless, so remove them.
    prunedIds <- liftIO $ withConnection pool deleteEmptyClusters
    unless (null prunedIds) $
      $(logTM) InfoS $ logStr $ ("Pruned " <> show (length prunedIds) <> " empty cluster(s): " <> show prunedIds :: Text)

    -- Reload clusters to get updated state
    allClusters <- liftIO $ withConnection pool getAllClusters

    now <- liftIO (getNow scClock)
    config <- liftIO $ readTVarIO scConfigVar
    let identifyConfig = mkIdentifyConfig (library config) (musicbrainz config)
    let (alreadyMatched, needsId) = partitionClusters now identifyConfig allClusters
    let totalGroups = length allClusters

    -- Affected = actually changed cluster IDs that also need identification
    let touchedSet = catMaybes touchedIds
    let needsIdSet = mapMaybe clusterId needsId
    let affectedIds = filter (`elem` needsIdSet) touchedSet

    $(logTM) InfoS $ logStr $ ("Created/updated " <> show totalGroups <> " clusters" :: Text)
    $(logTM) InfoS $ logStr $ (show (length alreadyMatched) <> " albums already matched/recently tried, " <>
                               show (length needsId) <> " need identification (" <>
                               show (length affectedIds) <> " affected)" :: Text)

    -- Emit ClustersGenerated event
    liftIO $ publishAndLog bus le "grouper" $ ClustersGenerated
      { totalGroups = totalGroups
      , alreadyMatched = length alreadyMatched
      , needsIdentification = length needsId
      , affectedClusterIds = affectedIds
      }

