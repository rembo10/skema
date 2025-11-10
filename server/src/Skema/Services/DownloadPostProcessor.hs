{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Skema.Services.DownloadPostProcessor
  ( startPostProcessor
  ) where

import Control.Concurrent.STM (readTChan)
import Control.Concurrent.Async (async)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import Katip

import Skema.Services.Types (ServiceContext(..))
import Skema.Database.Connection (ConnectionPool, withConnection, queryRows, executeQuery)
import Skema.Database.Types (DownloadRecord(..), downloadStatusToText)
import qualified Skema.Database.Types as DB
import Skema.Events.Types (Event(..), EventEnvelope(..))
import Skema.Events.Bus (EventBus, subscribe, publishAndLog)

-- | Start the post-processor service that handles completed downloads
startPostProcessor :: ServiceContext -> IO ()
startPostProcessor ServiceContext{..} = do
  runKatipContextT scLogEnv () "post-processor" $ do
    $(logTM) InfoS "Starting download post-processor service"

  -- Subscribe to download completed events
  eventChan <- STM.atomically $ subscribe scEventBus

  -- Spawn event processing loop in background thread
  _ <- async $ runKatipContextT scLogEnv () "post-processor" $ forever $ do
    envelope <- liftIO $ STM.atomically $ readTChan eventChan
    case envelopeEvent envelope of
      DownloadCompleted{..} -> do
        $(logTM) InfoS $ logStr $ "Post-processing completed download: " <> downloadTitle
        liftIO $ processCompletedDownload scLogEnv scEventBus scDbPool downloadId

      _ -> pure ()  -- Ignore other events

  pure ()

-- | Process a completed download
processCompletedDownload :: LogEnv -> EventBus -> ConnectionPool -> Int64 -> IO ()
processCompletedDownload le bus pool dlId = do
  -- Get download record from database
  downloads <- withConnection pool $ \conn ->
    queryRows conn
      "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
      \download_client_id, status, download_path, title, size_bytes, quality, \
      \format, seeders, progress, error_message, queued_at, started_at, \
      \completed_at, imported_at, updated_at, \
      \matched_cluster_id, library_path \
      \FROM downloads WHERE id = ?"
      (Only dlId) :: IO [DownloadRecord]

  runKatipContextT le () "post-processor" $ case downloads of
    [] -> do
      $(logTM) ErrorS $ logStr $ "Download not found: " <> T.pack (show dlId)

    (download:_) -> do
      case DB.downloadPath download of
        Nothing -> do
          $(logTM) ErrorS $ logStr $ "Completed download has no path: " <> DB.downloadTitle download
          liftIO $ markDownloadAsFailed pool dlId "No download path"

        Just path -> do
          $(logTM) InfoS $ logStr $ "Verifying download at: " <> path

          -- TODO: Scan the downloaded files and extract metadata
          -- For now, we'll skip verification and mark as imported
          -- In the future, this should:
          -- 1. Scan the download directory for audio files
          -- 2. Read metadata from files
          -- 3. Use identifyAlbum to verify it matches the wanted album
          -- 4. Move files to library if verification succeeds
          -- 5. Update database to mark as imported

          $(logTM) InfoS $ logStr $ "Marking download as imported: " <> DB.downloadTitle download
          liftIO $ markDownloadAsImported le bus pool dlId path

-- | Mark a download as failed
markDownloadAsFailed :: ConnectionPool -> Int64 -> Text -> IO ()
markDownloadAsFailed pool dlId errorMsg = do
  withConnection pool $ \conn ->
    executeQuery conn
      "UPDATE downloads SET status = ?, error_message = ? WHERE id = ?"
      (downloadStatusToText DB.DownloadFailed, errorMsg, dlId)

-- | Mark a download as imported
markDownloadAsImported :: LogEnv -> EventBus -> ConnectionPool -> Int64 -> Text -> IO ()
markDownloadAsImported le bus pool dlId _path = do
  now <- getCurrentTime

  -- Get download title for the event
  downloads <- withConnection pool $ \conn ->
    queryRows conn
      "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
      \download_client_id, status, download_path, title, size_bytes, quality, \
      \format, seeders, progress, error_message, queued_at, started_at, \
      \completed_at, imported_at, updated_at, \
      \matched_cluster_id, library_path \
      \FROM downloads WHERE id = ?"
      (Only dlId) :: IO [DownloadRecord]

  case downloads of
    (download:_) -> do
      withConnection pool $ \conn ->
        executeQuery conn
          "UPDATE downloads SET status = ?, imported_at = ? WHERE id = ?"
          (downloadStatusToText DB.DownloadImported, now, dlId)

      publishAndLog bus le "post-processor" $ DownloadImported
        { downloadId = dlId
        , downloadTitle = DB.downloadTitle download
        }

    [] -> pure ()
