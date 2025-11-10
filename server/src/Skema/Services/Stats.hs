{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Stats service - computes and emits library statistics.
--
-- This service listens for events that modify library stats and emits
-- STATS_UPDATED events with fresh statistics.
module Skema.Services.Stats
  ( startStatsService
  ) where

import Skema.Services.Dependencies (StatsDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getLibraryStats)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try)
import Control.Concurrent (threadDelay)
import Katip

-- | Debounce interval in microseconds (2 seconds).
-- Stats updates will be batched to avoid hammering the database.
debounceIntervalMicros :: Int
debounceIntervalMicros = 2000000

-- | Start the stats service.
--
-- This service listens for events that modify stats and emits STATS_UPDATED events.
-- Uses debouncing to batch updates and reduce database load.
-- Exceptions are caught and logged to prevent the service from crashing.
startStatsService :: StatsDeps -> Maybe Text -> IO (Async (), Async ())
startStatsService deps libraryPath = do
  chan <- STM.atomically $ subscribe (statsEventBus deps)

  -- TVar to track whether a stats update is pending
  updatePending <- STM.newTVarIO False

  -- Event listener thread - marks stats as needing update
  listenerHandle <- async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      -- Events that change stats - mark as needing update
      MetadataReadComplete{} -> STM.atomically $ writeTVar updatePending True
      ClustersGenerated{} -> STM.atomically $ writeTVar updatePending True
      ClusterIdentified{} -> STM.atomically $ writeTVar updatePending True
      TrackDiffsGenerated{} -> STM.atomically $ writeTVar updatePending True
      MetadataDiffApplied{} -> STM.atomically $ writeTVar updatePending True
      GroupedDiffsApplied{} -> STM.atomically $ writeTVar updatePending True
      MetadataWriteCompleted{} -> STM.atomically $ writeTVar updatePending True
      _ -> pure ()  -- Ignore other events

  -- Stats updater thread - checks periodically and emits if needed
  updaterHandle <- async $ forever $ do
    threadDelay debounceIntervalMicros

    -- Check if update is needed
    needsUpdate <- STM.atomically $ do
      pending <- readTVar updatePending
      when pending $ writeTVar updatePending False
      pure pending

    -- Emit stats update if needed
    when needsUpdate $ do
      result <- try $ emitStatsUpdate deps libraryPath
      case result of
        Left (e :: SomeException) -> do
          let le = statsLogEnv deps
          runKatipContextT le () "stats.error" $ do
            $(logTM) ErrorS $ logStr $ ("Exception in stats service: " <> show e :: Text)
        Right () -> pure ()

  pure (listenerHandle, updaterHandle)

-- | Compute current stats and emit STATS_UPDATED event.
emitStatsUpdate :: StatsDeps -> Maybe Text -> IO ()
emitStatsUpdate StatsDeps{..} _libraryPath = do
  let le = statsLogEnv
  let pool = statsDbPool
  let bus = statsEventBus
  let initialContext = ()
  let initialNamespace = "services.stats"

  runKatipContextT le initialContext initialNamespace $ do
    -- Query database for fresh stats
    (totalFiles, totalAlbums, totalArtists, matchedFiles, unmatchedFiles, accuracy, totalDiffs, totalSize, totalRuntime) <-
      liftIO $ withConnection pool getLibraryStats

    -- Emit STATS_UPDATED event
    liftIO $ publishAndLog bus le "stats" $ StatsUpdated
      { totalFiles = totalFiles
      , totalAlbums = totalAlbums
      , totalArtists = totalArtists
      , matchedFiles = matchedFiles
      , unmatchedFiles = unmatchedFiles
      , metadataAccuracy = accuracy
      , totalDiffs = totalDiffs
      , librarySize = fromIntegral totalSize
      , totalRuntime = round totalRuntime
      }
