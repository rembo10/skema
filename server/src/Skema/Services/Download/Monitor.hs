{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Download monitoring and status updates.
--
-- This module handles polling download clients for status updates and
-- emitting progress events.
module Skema.Services.Download.Monitor
  ( runDownloadMonitor
  ) where

import Skema.Services.Dependencies (DownloadDeps(..))
import Skema.Services.Download.Client (DownloadClientInstance, createClientInstances)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.Config.Types (Config(..), DownloadConfig(..))
import Skema.DownloadClient.Types (DownloadClientAPI(..))
import qualified Skema.DownloadClient.Types as DC
import Control.Concurrent.Async (async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime)
import qualified Data.Map.Strict as Map
import Database.SQLite.Simple (Only(..))
import Skema.Database.Types (DownloadRecord(..))
import qualified Skema.Database.Types as DB
import Skema.Database.Utils (downloadStatusToText)
import Katip

-- ============================================================================
-- CONSTANTS
-- ============================================================================

-- | Poll interval when downloads are active (1 second in microseconds)
activePollingIntervalMicros :: Int
activePollingIntervalMicros = 1000000

-- | Poll interval when no downloads active (5 seconds in microseconds)
idlePollingIntervalMicros :: Int
idlePollingIntervalMicros = 5000000

-- ============================================================================
-- DOWNLOAD MONITOR
-- ============================================================================

-- | Run the download monitor sub-process.
--
-- Uses an event-driven approach: loads initial state on startup, then subscribes
-- to download events to know when to start/stop polling download clients.
-- When downloads are active, polls every 1 second and emits progress events.
-- Download client instances are created once at startup and recreated only when
-- the config changes (via ConfigUpdated event).
runDownloadMonitor :: DownloadDeps -> IO ()
runDownloadMonitor DownloadDeps{..} = do
  runKatipContextT dlLogEnv () "download.monitor" $ do
    let bus = dlEventBus
        pool = dlDbPool
        httpClient = dlHttpClient

    $(logTM) InfoS "Starting download monitor"

    -- Create initial client instances from config
    config <- liftIO $ STM.atomically $ STM.readTVar dlConfigVar
    let downloadCfg = download config
    initialClients <- liftIO $ createClientInstances httpClient downloadCfg

    $(logTM) InfoS $ logStr $ ("Created " <> show (length initialClients) <> " initial client instances" :: Text)
    forM_ initialClients $ \(clientName, _) ->
      $(logTM) InfoS $ logStr $ ("  - Client: '" <> clientName <> "'" :: Text)

    -- Store clients in a TVar so they can be updated when config changes
    clientsVar <- liftIO $ STM.atomically $ STM.newTVar initialClients

    -- Check initial state: are there any active downloads?
    initialCount <- liftIO $ withConnection pool $ \conn -> do
      results <- queryRows conn
        "SELECT COUNT(*) FROM downloads WHERE status IN ('queued', 'downloading')"
        () :: IO [Only Int]
      case results of
        [Only count] -> pure count
        _ -> pure 0

    $(logTM) InfoS $ logStr $ ("Found " <> show initialCount <> " active downloads on startup" :: Text)

    -- Create a TVar to track the count of active downloads
    activeCountVar <- liftIO $ STM.atomically $ STM.newTVar initialCount

    -- Subscribe to events to maintain the count and reload clients on config changes
    eventChan <- liftIO $ STM.atomically $ subscribe bus
    _ <- liftIO $ async $ forever $ do
      envelope <- STM.atomically $ readTChan eventChan
      case envelopeEvent envelope of
        DownloadStarted{} -> do
          -- Increment active download count
          STM.atomically $ STM.modifyTVar' activeCountVar (+1)

        DownloadCompleted{} -> do
          -- Decrement active download count
          STM.atomically $ STM.modifyTVar' activeCountVar (\n -> max 0 (n - 1))

        DownloadFailed{} -> do
          -- Decrement active download count
          STM.atomically $ STM.modifyTVar' activeCountVar (\n -> max 0 (n - 1))

        ConfigUpdated{} -> do
          -- Config changed - recreate download client instances
          runKatipContextT dlLogEnv () "download.monitor" $ do
            $(logTM) InfoS "Config updated, recreating download client instances"
          newConfig <- STM.atomically $ STM.readTVar dlConfigVar
          let newDownloadCfg = download newConfig
          newClients <- createClientInstances httpClient newDownloadCfg
          runKatipContextT dlLogEnv () "download.monitor" $ do
            $(logTM) InfoS $ logStr $ ("Created " <> show (length newClients) <> " client instances from updated config" :: Text)
          STM.atomically $ STM.writeTVar clientsVar newClients

        _ -> pure ()  -- Ignore other events

    -- Spawn monitoring loop in background thread
    _ <- liftIO $ async $ runKatipContextT dlLogEnv () "download.monitor" $ forever $ do
      -- Read current client instances (only changes when config changes)
      clients <- liftIO $ STM.atomically $ STM.readTVar clientsVar

      -- Check if there are active downloads
      activeCount <- liftIO $ STM.atomically $ STM.readTVar activeCountVar

      $(logTM) DebugS $ logStr $ ("Monitor loop: activeCount=" <> show activeCount <> ", clients=" <> show (length clients) :: Text)

      if activeCount > 0 && not (null clients)
        then do
          -- Active downloads exist - poll every 1 second for real-time updates
          $(logTM) DebugS $ logStr $ ("Checking " <> show (length clients) <> " clients..." :: Text)
          forM_ clients $ \(clientName, client) ->
            liftIO $ checkAndUpdateDownloads dlLogEnv bus pool dlProgressMap clientName client

          -- Sleep briefly to provide real-time updates to frontend
          liftIO $ threadDelay activePollingIntervalMicros

        else do
          -- No active downloads or no clients - just sleep and check again later
          $(logTM) DebugS "No active downloads or no clients, sleeping..."
          liftIO $ threadDelay idlePollingIntervalMicros

    pure ()
  pure ()

-- | Check all downloads for a specific client and update database
checkAndUpdateDownloads :: LogEnv -> EventBus -> ConnectionPool -> STM.TVar (Map.Map Int64 (Double, Text)) -> Text -> DownloadClientInstance -> IO ()
checkAndUpdateDownloads le bus pool progressMap clientName client = do
  -- Get all active downloads from database for this client
  activeDownloads <- withConnection pool $ \conn ->
    queryRows conn
      "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
      \download_client_id, status, download_path, title, size_bytes, quality, \
      \format, seeders, progress, error_message, queued_at, started_at, \
      \completed_at, imported_at, updated_at, \
      \matched_cluster_id, library_path \
      \FROM downloads \
      \WHERE download_client = ? AND status IN ('queued', 'downloading')"
      (Only clientName) :: IO [DownloadRecord]

  forM_ activeDownloads $ \download -> do
    -- Get current status from client
    case DB.downloadClientId download of
      Nothing -> runKatipContextT le () "download.monitor" $
        $(logTM) WarningS $ logStr $ "Download has no client ID: " <> DB.downloadTitle download
      Just clientId -> do
        statusResult <- getDownloadStatus client clientId

        case statusResult of
          Left err -> runKatipContextT le () "download.monitor" $ do
            $(logTM) ErrorS $ logStr $ ("Failed to get download status for " <> DB.downloadTitle download <> ": " <> err :: Text)

          Right downloadInfo -> do
            let oldStatus = DB.downloadStatus download
                newStatus = mapClientStatusToDBStatus (DC.diStatus downloadInfo)
                oldProgress = DB.downloadProgress download
                newProgress = DC.diProgress downloadInfo
                downloadIdVal = fromMaybe 0 (DB.downloadId download)

            -- Only update database when status changes
            when (oldStatus /= newStatus) $ do
              now <- getCurrentTime
              withConnection pool $ \conn -> do
                executeQuery conn
                  "UPDATE downloads SET status = ?, download_path = ?, \
                  \error_message = ?, size_bytes = ?, started_at = COALESCE(started_at, ?), \
                  \completed_at = ? WHERE id = ?"
                  ( downloadStatusToText newStatus
                  , DC.diDownloadPath downloadInfo
                  , DC.diErrorMessage downloadInfo
                  , DC.diSizeBytes downloadInfo
                  , if newStatus == DB.DownloadDownloading then Just now else Nothing
                  , if newStatus == DB.DownloadCompleted then Just now else Nothing
                  , DB.downloadId download
                  )

            -- Update in-memory progress map for active downloads (store progress + display status)
            when (newStatus == DB.DownloadDownloading) $ do
              let displayStatus = clientStatusToDisplayText (DC.diStatus downloadInfo)
              STM.atomically $ STM.modifyTVar' progressMap $ \m ->
                Map.insert downloadIdVal (newProgress, displayStatus) m

            -- Remove from progress map when download is complete/failed
            when (newStatus == DB.DownloadCompleted || newStatus == DB.DownloadFailed) $ do
              STM.atomically $ STM.modifyTVar' progressMap $ \m ->
                Map.delete downloadIdVal m

            -- Emit progress event if download is active and progress changed significantly
            -- (emit if progress increased by at least 1% to avoid spamming)
            when (newStatus == DB.DownloadDownloading && abs (newProgress - oldProgress) >= 0.01) $ do
              publishAndLog bus le "download" $ DownloadProgress
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadProgress = newProgress
                , downloadSizeBytes = DC.diSizeBytes downloadInfo
                , downloadedBytes = DC.diDownloadedBytes downloadInfo
                }

            -- If status changed to completed, emit event for post-processing
            when (oldStatus /= DB.DownloadCompleted && newStatus == DB.DownloadCompleted) $ do
              runKatipContextT le () "download.monitor" $
                $(logTM) InfoS $ logStr $ "Download completed: " <> DB.downloadTitle download
              publishAndLog bus le "download" $ DownloadCompleted
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadPath = DB.downloadPath download
                }

            -- If download failed, emit event
            when (oldStatus /= DB.DownloadFailed && newStatus == DB.DownloadFailed) $ do
              runKatipContextT le () "download.monitor" $
                $(logTM) ErrorS $ logStr $ "Download failed: " <> DB.downloadTitle download
              publishAndLog bus le "download" $ DownloadFailed
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadError = DB.downloadErrorMessage download
                }

            -- If download was cancelled, emit failed event (treat as terminal state)
            when (oldStatus /= DB.DownloadCancelled && newStatus == DB.DownloadCancelled) $ do
              runKatipContextT le () "download.monitor" $
                $(logTM) WarningS $ logStr $ "Download cancelled: " <> DB.downloadTitle download
              publishAndLog bus le "download" $ DownloadFailed
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadError = Just "Download cancelled"
                }

-- | Map download client status to database status
-- For persistence, we only care about: downloading (active), completed, or failed
mapClientStatusToDBStatus :: DC.DownloadStatus -> DB.DownloadStatus
mapClientStatusToDBStatus DC.DSQueued = DB.DownloadDownloading      -- Active download
mapClientStatusToDBStatus DC.DSDownloading = DB.DownloadDownloading -- Active download
mapClientStatusToDBStatus DC.DSPaused = DB.DownloadDownloading      -- Still active, just paused
mapClientStatusToDBStatus DC.DSCompleted = DB.DownloadCompleted     -- Terminal state
mapClientStatusToDBStatus DC.DSFailed = DB.DownloadFailed           -- Terminal state
mapClientStatusToDBStatus DC.DSCancelled = DB.DownloadFailed        -- Terminal state (treat as failed)

-- | Convert client status to display text for frontend
clientStatusToDisplayText :: DC.DownloadStatus -> Text
clientStatusToDisplayText DC.DSQueued = "queued"
clientStatusToDisplayText DC.DSDownloading = "downloading"
clientStatusToDisplayText DC.DSPaused = "paused"
clientStatusToDisplayText DC.DSCompleted = "completed"
clientStatusToDisplayText DC.DSFailed = "failed"
clientStatusToDisplayText DC.DSCancelled = "cancelled"
