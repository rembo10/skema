{-# LANGUAGE LambdaCase #-}
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
import Skema.Services.EventLoop (JobMode(..), runEventService)
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
  -- TVar to track whether a stats update is pending
  updatePending <- STM.newTVarIO False
  let markPending = STM.atomically $ writeTVar updatePending True

  -- Event listener - marks stats as needing update
  listenerHandle <- runEventService (statsLogEnv deps) (statsEventBus deps) "services.stats" Sequential $ \case
    MetadataReadComplete{} -> Just markPending
    ClustersGenerated{} -> Just markPending
    ClusterIdentified{} -> Just markPending
    TrackDiffsGenerated{} -> Just markPending
    MetadataDiffApplied{} -> Just markPending
    GroupedDiffsApplied{} -> Just markPending
    MetadataWriteCompleted{} -> Just markPending
    _ -> Nothing

  -- Stats updater thread - blocks until an update is pending, then lets a
  -- burst of events coalesce for the debounce interval before recomputing
  updaterHandle <- async $ forever $ do
    STM.atomically $ readTVar updatePending >>= flip unless STM.retry
    threadDelay debounceIntervalMicros
    STM.atomically $ writeTVar updatePending False

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
    (totalFiles, totalAlbums, totalArtists, matchedFiles, unmatchedFiles, accuracy, totalDiffs, totalSize, totalRuntime, catInLibrary, catWanted) <-
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
      , catalogInLibrary = catInLibrary
      , catalogWanted = catWanted
      }
