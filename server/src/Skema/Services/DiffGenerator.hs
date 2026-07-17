{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Metadata diff generator service.
--
-- This service listens for CLUSTER_IDENTIFIED events and generates metadata diffs
-- by comparing file metadata with MusicBrainz data.
module Skema.Services.DiffGenerator
  ( startDiffGeneratorService
  ) where

import Skema.Services.Types (ServiceContext(..))
import Skema.Services.EventLoop (JobMode(..), runEventService)
import Skema.Domain.Metadata (metadataRecordToMonatone)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getClusterWithTracks, getClusterById, insertMetadataDiff, deleteMetadataDiffsForTrack, computeMetadataDiffs)
import Skema.Database.Types (LibraryTrackMetadataRecord(..), ClusterRecord(..))
import Skema.MusicBrainz.Client (getRelease)
import Skema.MusicBrainz.Types (MBTrack(..), MBID(..), mbReleaseTracks)
import Control.Concurrent.Async (Async)
import Control.Monad ()
import Katip

-- | Start the diff generator service.
--
-- This service listens for CLUSTER_IDENTIFIED and TRACKS_REMATCHED events and generates metadata diffs.
startDiffGeneratorService :: ServiceContext -> IO (Async ())
startDiffGeneratorService deps =
  runEventService (scLogEnv deps) (scEventBus deps) "services.diff-generator" Sequential $ \case
    ClusterIdentified cid releaseId _ _confidence _trackCount ->
      Just $ handleClusterIdentified deps cid releaseId
    TracksRematched cid _trackCount ->
      Just $ handleTracksRematched deps cid
    _ -> Nothing

-- | Handle a cluster identified event.
handleClusterIdentified :: ServiceContext -> Int64 -> Text -> IO ()
handleClusterIdentified ServiceContext{..} clusterId releaseId = do
  let le = scLogEnv
  let pool = scDbPool
  let bus = scEventBus
  let initialContext = ()
  let initialNamespace = "services.diff-generator"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "cluster_id" clusterId) $ do
      katipAddContext (sl "release_id" releaseId) $ do
        -- Get cluster with tracks from database
        maybeClusterData <- liftIO $ withConnection pool $ \conn ->
          getClusterWithTracks conn clusterId

        case maybeClusterData of
          Nothing -> do
            $(logTM) WarningS $ logStr ("Cluster not found: " <> show clusterId :: Text)
            pure ()

          Just (_, tracks) -> do
            -- Fetch MusicBrainz release data using shared client
            let mbEnv = scMBClientEnv
            mbReleaseResult <- liftIO $ getRelease mbEnv (MBID releaseId)

            case mbReleaseResult of
              Left err -> do
                $(logTM) ErrorS $ logStr $ ("Failed to lookup release " <> releaseId <> ": " <> show err :: Text)
                pure ()

              Right mbRelease -> do
                -- Generate diffs for each track
                liftIO $ withConnection pool $ \conn -> do
                  forM_ tracks $ \(trackId, _path, metadata, _mbRecId, _mbRecTitle) -> do
                    -- Convert metadata record to Monatone format
                    let fileMeta = metadataRecordToMonatone metadata

                    -- Find matching MB track by position
                    let maybeTrackNum = metaTrackNumber metadata
                    case maybeTrackNum of
                      Nothing -> pure ()  -- Skip tracks without track number
                      Just trackNum -> do
                        -- Find MB track with matching position
                        let mbTracks = mbReleaseTracks mbRelease
                        let matchingTrack = find (\t -> mbTrackPosition t == trackNum) mbTracks

                        case matchingTrack of
                          Nothing -> pure ()  -- No matching track found
                          Just mbTrack -> do
                            -- Compute diffs
                            let diffs = computeMetadataDiffs fileMeta mbTrack mbRelease

                            -- Delete old diffs for this track
                            deleteMetadataDiffsForTrack conn trackId

                            -- Insert new diffs
                            forM_ diffs $ \(fieldName, fileVal, mbVal) -> do
                              insertMetadataDiff conn trackId fieldName fileVal mbVal

                            -- Emit TRACK_DIFFS_GENERATED event once per track
                            when (not (null diffs)) $ do
                              publishAndLog bus le "diff-generator" $ TrackDiffsGenerated
                                { diffTrackId = trackId
                                , diffClusterId = clusterId
                                , diffCount = length diffs
                                }

-- | Handle a tracks rematched event.
-- This regenerates diffs for the cluster after manual track rematch.
handleTracksRematched :: ServiceContext -> Int64 -> IO ()
handleTracksRematched ServiceContext{..} clusterId = do
  let le = scLogEnv
  let pool = scDbPool

  runKatipContextT le () "services.diff-generator" $ do
    katipAddContext (sl "cluster_id" clusterId) $ do
      -- Get cluster to find its release ID
      maybeCluster <- liftIO $ withConnection pool $ \conn ->
        getClusterById conn clusterId

      case maybeCluster of
        Nothing -> do
          $(logTM) WarningS $ logStr ("Cluster not found: " <> show clusterId :: Text)
          pure ()

        Just cluster ->
          case clusterMBReleaseId cluster of
            Nothing -> do
              $(logTM) InfoS $ logStr ("Cluster " <> show clusterId <> " has no MB release assigned, skipping diff generation" :: Text)
              pure ()

            Just releaseId -> do
              $(logTM) InfoS $ logStr ("Regenerating diffs for cluster " <> show clusterId <> " after track rematch" :: Text)
              -- Reuse the existing handler
              liftIO $ handleClusterIdentified ServiceContext{..} clusterId releaseId
