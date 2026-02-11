{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Grouper service - groups files by album.
module Skema.Services.Grouper
  ( startGrouperService
  , handleMetadataReadComplete
  ) where

import Skema.Services.Dependencies (GrouperDeps(..))
import Skema.Services.Common (metadataRecordToMonatone)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getAllClusters, getAllTracks, getMetadataForTrack, getTrackByPath, computeClusterHash, findClusterByHash, createCluster, updateTrackCluster, updateClusterQuality)
import Skema.Database.Types (ClusterRecord(..), LibraryTrackRecord(..))
import Skema.MusicBrainz.Matching (groupFilesByRelease)
import Skema.MusicBrainz.Types (FileGroup(..))
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try)
import Data.List (partition)
import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, addUTCTime)
import Katip

-- | Number of hours to wait before retrying failed MusicBrainz identification
retryIntervalHours :: NominalDiffTime
retryIntervalHours = 24

-- | Start the grouper service.
--
-- This service listens for METADATA_READ_COMPLETE events and groups files by album.
-- It runs after metadata has been read and stored in the database.
-- Returns the async handle for graceful shutdown.
startGrouperService :: GrouperDeps -> IO (Async ())
startGrouperService deps = do
  chan <- STM.atomically $ subscribe (groupEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      MetadataReadComplete{..} -> do
        -- Process each grouping request asynchronously with error handling
        _ <- async $ do
          result <- try $ handleMetadataReadComplete deps filesProcessed
          case result of
            Left (e :: SomeException) -> do
              let le = groupLogEnv deps
              runKatipContextT le () "grouper.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception during grouping: " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle metadata read complete event.
-- Groups all files in the library by album (not just changed files, since
-- grouping needs to see the full album to create proper clusters).
handleMetadataReadComplete :: GrouperDeps -> Int -> IO ()
handleMetadataReadComplete GrouperDeps{..} fileCount = do
  let le = groupLogEnv
  let pool = groupDbPool
  let bus = groupEventBus
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

        -- Get track IDs for this group
        trackIds <- forM (fgFiles fg) $ \(path, _) -> do
          maybeTrack <- getTrackByPath conn path
          pure $ maybeTrack >>= trackId

        let validTrackIds = catMaybes trackIds

        -- Assign tracks to cluster
        when (not (null validTrackIds)) $ do
          updateTrackCluster conn cid validTrackIds
          -- Compute and store cluster quality from track audio metadata
          updateClusterQuality conn cid

        pure cid

    -- Reload clusters to get updated state
    allClusters <- liftIO $ withConnection pool getAllClusters

    now <- liftIO getCurrentTime
    let retryThreshold = addUTCTime (negate (retryIntervalHours * 3600)) now

    let (alreadyMatched, needsId) = partitionClusters retryThreshold allClusters
    let totalGroups = length allClusters

    -- Affected = touched cluster IDs that also need identification
    let needsIdSet = mapMaybe clusterId needsId
    let affectedIds = filter (`elem` needsIdSet) touchedIds

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

-- | Partition clusters into those that are already matched and those that need identification.
partitionClusters :: UTCTime -> [ClusterRecord] -> ([ClusterRecord], [ClusterRecord])
partitionClusters retryThreshold clusters =
  let needsIdentification cluster = case clusterMBReleaseId cluster of
        Just _ -> False  -- Already has MusicBrainz data
        Nothing -> case clusterLastIdentifiedAt cluster of
          Just lastTried | lastTried > retryThreshold -> False  -- Tried recently, skip
          _ -> True  -- Needs identification or retry interval passed

      (needs, matched) = partition needsIdentification clusters
  in (matched, needs)
