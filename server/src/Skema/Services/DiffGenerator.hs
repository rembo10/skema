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

import Skema.Services.Dependencies (DiffGeneratorDeps(..))
import Skema.Services.Common (metadataRecordToMonatone)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getClusterWithTracks, insertMetadataDiff, deleteMetadataDiffsForTrack, computeMetadataDiffs)
import Skema.Database.Types (LibraryTrackMetadataRecord(..))
import Skema.MusicBrainz.Client (getRelease)
import Skema.MusicBrainz.Types (MBRelease(..), MBTrack(..), MBID(..))
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Monad ()
import Control.Exception (try)
import Katip

-- | Start the diff generator service.
--
-- This service listens for CLUSTER_IDENTIFIED events and generates metadata diffs.
startDiffGeneratorService :: DiffGeneratorDeps -> IO (Async ())
startDiffGeneratorService deps = do
  chan <- STM.atomically $ subscribe (diffEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    -- Process each event asynchronously to avoid blocking the event loop
    case envelopeEvent envelope of
      ClusterIdentified cid releaseId _ _confidence _trackCount -> do
        _ <- async $ do
          result <- try $ handleClusterIdentified deps cid releaseId
          case result of
            Left (e :: SomeException) -> do
              let le = diffLogEnv deps
              runKatipContextT le () "diff-generator.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception processing cluster " <> show cid <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle a cluster identified event.
handleClusterIdentified :: DiffGeneratorDeps -> Int64 -> Text -> IO ()
handleClusterIdentified DiffGeneratorDeps{..} clusterId releaseId = do
  let le = diffLogEnv
  let pool = diffDbPool
  let bus = diffEventBus
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
            let mbEnv = diffMBClient
            mbReleaseResult <- liftIO $ getRelease mbEnv (MBID releaseId)

            case mbReleaseResult of
              Left err -> do
                $(logTM) ErrorS $ logStr $ ("Failed to lookup release " <> releaseId <> ": " <> show err :: Text)
                pure ()

              Right mbRelease -> do
                -- Generate diffs for each track
                liftIO $ withConnection pool $ \conn -> do
                  forM_ tracks $ \(trackId, _path, metadata) -> do
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
