{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Debounced file system watching.
--
-- This module provides file system watching with a rolling window debounce
-- mechanism. Instead of emitting individual events for each file change,
-- it waits for a period of silence and then emits a single FileSystemDiff
-- containing all changes.
--
-- This is particularly useful for bulk operations like copying many files,
-- where you want a single notification after the operation completes rather
-- than hundreds of individual events.
module Skema.FileSystem.Watcher
  ( -- * Watching
    watchDirectory
  , watchDirectoryWithEvents
  , StopListening
    -- * Configuration
  , defaultDebounceMs
  ) where

import Skema.Core.Library
import Skema.FileSystem.Scanner (scanDirectory)
import Skema.Events.Bus (EventBus, publishAndLog)
import Skema.Events.Types (Event(..))
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Katip

-- | Default debounce window in milliseconds (5 seconds).
--
-- After file system events stop for this duration, a diff will be emitted.
defaultDebounceMs :: Int
defaultDebounceMs = 5000

-- | Watch a directory for changes with debouncing.
--
-- The watcher uses a rolling window approach: any file system event resets
-- the timer. Only after 'debounceMs' milliseconds of silence will a
-- 'FileSystemDiff' be computed and passed to the callback.
--
-- Example:
--
-- @
-- stop <- watchDirectory root 5000 $ \diff -> do
--   putStrLn $ "Changes detected: " <> show diff
-- -- ... later ...
-- stop  -- Stop watching
-- @
--
-- The rolling window behavior:
--
-- * Time 0ms: file1 added → timer starts
-- * Time 500ms: file2 added → timer resets
-- * Time 2000ms: file3 added → timer resets
-- * Time 7000ms: 5000ms of silence → emit diff with all 3 files
watchDirectory
  :: OsPath
  -- ^ Root directory to watch
  -> Int
  -- ^ Debounce window in milliseconds
  -> (FileSystemDiff -> IO ())
  -- ^ Callback invoked with file system diffs
  -> IO StopListening
  -- ^ Action to stop watching
watchDirectory root debounceMs callback =
  watchDirectoryWithEvents root debounceMs callback Nothing Nothing

-- | Watch a directory with optional event bus and logging support.
--
-- This is the same as 'watchDirectory' but with optional event bus integration.
-- When an event bus and log environment are provided, the watcher will emit
-- FileSystemDiffGenerated events and log at DEBUG level.
watchDirectoryWithEvents
  :: OsPath
  -- ^ Root directory to watch
  -> Int
  -- ^ Debounce window in milliseconds
  -> (FileSystemDiff -> IO ())
  -- ^ Callback invoked with file system diffs
  -> Maybe EventBus
  -- ^ Optional event bus for emitting events
  -> Maybe LogEnv
  -- ^ Optional log environment for logging
  -> IO StopListening
  -- ^ Action to stop watching
watchDirectoryWithEvents root debounceMs callback mBus mLogEnv = do
  -- Convert OsPath to FilePath for fsnotify
  rootPath <- OP.decodeUtf root

  -- Get initial snapshot
  initialSnapshot <- scanDirectory root
  snapshotRef <- newIORef initialSnapshot

  -- Track last event time (Nothing = no pending events)
  lastEventTime <- newIORef (Nothing :: Maybe UTCTime)

  mgr <- startManager

  -- Debounce thread: checks every 100ms if the debounce window has elapsed
  debounceThread <- async $ forever $ do
    threadDelay 100000  -- 100ms

    mLastTime <- readIORef lastEventTime
    now <- getCurrentTime

    -- Check if debounce window has elapsed
    let shouldProcess = case mLastTime of
          Nothing -> False
          Just lastTime ->
            let elapsedMs = round (diffUTCTime now lastTime * 1000)
            in elapsedMs >= debounceMs

    when shouldProcess $ do
      -- Reset event time
      writeIORef lastEventTime Nothing

      -- Compute diff by rescanning
      oldSnapshot <- readIORef snapshotRef
      newSnapshot <- scanDirectory root

      let diff = computeDiff oldSnapshot newSnapshot

      -- Emit if changes detected
      unless (isEmpty diff) $ do
        -- Log and emit event
        let addedCount = length (diffAdded diff)
        let modifiedCount = length (diffModified diff)
        let deletedCount = length (diffDeleted diff)

        -- Log to console if log environment provided
        forM_ mLogEnv $ \le -> do
          let initialContext = ()
          let initialNamespace = "watcher"
          runKatipContextT le initialContext initialNamespace $ do
            $(logTM) InfoS $ logStr $
              ("FileSystem changes detected: " <> show addedCount <> " added, " <>
               show modifiedCount <> " modified, " <> show deletedCount <> " deleted" :: Text)

        -- Emit event if bus provided
        forM_ ((,) <$> mBus <*> mLogEnv) $ \(bus, le) -> do
          publishAndLog bus le "watcher" $ FileSystemDiffGenerated
            { filesAdded = addedCount
            , filesModified = modifiedCount
            , filesDeleted = deletedCount
            }

        -- Call callback
        callback diff

      -- Update snapshot
      writeIORef snapshotRef newSnapshot

  -- Watch all file events in the tree
  watchStop <- watchTree mgr rootPath (const True) $ \_ -> do
    -- Any event resets the rolling window
    now <- getCurrentTime
    writeIORef lastEventTime (Just now)

  -- Return cleanup function
  let stop = do
        cancel debounceThread
        watchStop
        stopManager mgr

  pure stop
