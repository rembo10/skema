{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Trash cleanup service - periodically removes old files from trash.
--
-- This service runs in the background and cleans up files from the trash
-- directory that are older than the configured retention period.
module Skema.Services.TrashCleanup
  ( startTrashCleanupService
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Skema.Config.Types (Config(..), LibraryConfig(..), DownloadConfig(..), download, library)
import Skema.FileSystem.Trash (cleanupOldTrashFiles)
import Katip

-- | Start the trash cleanup service.
--
-- Runs daily to clean up files older than the retention period.
startTrashCleanupService :: LogEnv -> STM.TVar Config -> IO (Async ())
startTrashCleanupService le configVar = async $ do
  let initialContext = ()
  let initialNamespace = "trash-cleanup"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS "Starting trash cleanup service"

    forever $ do
      -- Read current config
      config <- liftIO $ STM.atomically $ STM.readTVar configVar

      let libConfig = library config
      let downloadCfg = download config

      -- Only run cleanup if trash is enabled and library path is configured
      case libraryPath libConfig of
        Nothing -> do
          $(logTM) DebugS "No library path configured, skipping trash cleanup"
          -- Sleep for 1 day
          liftIO $ threadDelay (24 * 60 * 60 * 1000000)

        Just _libPath | not (downloadUseTrash downloadCfg) -> do
          $(logTM) DebugS "Trash is disabled, skipping cleanup"
          -- Sleep for 1 day
          liftIO $ threadDelay (24 * 60 * 60 * 1000000)

        Just libPath -> do
          $(logTM) InfoS $ logStr $ ("Running trash cleanup (retention: " <> show (downloadTrashRetentionDays downloadCfg) <> " days)" :: Text)

          -- Clean up old files
          deletedCount <- liftIO $ cleanupOldTrashFiles libPath (downloadTrashRetentionDays downloadCfg)

          if deletedCount > 0
            then $(logTM) InfoS $ logStr $ ("Deleted " <> show deletedCount <> " old files from trash" :: Text)
            else $(logTM) DebugS "No old files to delete from trash"

          -- Sleep for 1 day before next cleanup
          liftIO $ threadDelay (24 * 60 * 60 * 1000000)
