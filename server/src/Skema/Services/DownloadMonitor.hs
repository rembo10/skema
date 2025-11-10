{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Skema.Services.DownloadMonitor
  ( startDownloadMonitor
  , DownloadClientInstance(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTVar)
import Control.Monad (forever, when, forM_)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import Katip

import Skema.Services.Types (ServiceContext(..))
import Skema.Config.Types (downloadClientTypeName, DownloadConfig(..), DownloadClient(..), DownloadClientType(..), dcType, dcEnabled, dcUrl, dcApiKey, dcCategory, dcDownloadDir, dcUsername, dcPassword, downloadNzbClient, downloadTorrentClient, SABnzbd(..), NZBGet(..), Transmission(..), QBittorrent(..))
import Skema.HTTP.Client (HttpClient)
import Skema.Database.Connection (ConnectionPool, withConnection, queryRows, executeQuery)
import Skema.Database.Types (DownloadRecord(..), downloadStatusToText)
import qualified Skema.Database.Types as DB
import Skema.DownloadClient.Types (DownloadClientAPI(..))
import qualified Skema.DownloadClient.Types as DC
import Skema.DownloadClient.SABnzbd
import Skema.DownloadClient.Transmission
import Skema.DownloadClient.QBittorrent
import Skema.Events.Types (Event(..))
import Skema.Events.Bus (EventBus, publishAndLog)

-- | Wrapper for different download client types
data DownloadClientInstance
  = SABInstance SABnzbdClient
  | TransmissionInstance TransmissionClient
  | QBittorrentInstance QBittorrentClient

instance DownloadClientAPI DownloadClientInstance where
  testConnection (SABInstance c) = testConnection c
  testConnection (TransmissionInstance c) = testConnection c
  testConnection (QBittorrentInstance c) = testConnection c

  addDownload (SABInstance c) = addDownload c
  addDownload (TransmissionInstance c) = addDownload c
  addDownload (QBittorrentInstance c) = addDownload c

  getDownloadStatus (SABInstance c) = getDownloadStatus c
  getDownloadStatus (TransmissionInstance c) = getDownloadStatus c
  getDownloadStatus (QBittorrentInstance c) = getDownloadStatus c

  getAllDownloads (SABInstance c) = getAllDownloads c
  getAllDownloads (TransmissionInstance c) = getAllDownloads c
  getAllDownloads (QBittorrentInstance c) = getAllDownloads c

  pauseDownload (SABInstance c) = pauseDownload c
  pauseDownload (TransmissionInstance c) = pauseDownload c
  pauseDownload (QBittorrentInstance c) = pauseDownload c

  resumeDownload (SABInstance c) = resumeDownload c
  resumeDownload (TransmissionInstance c) = resumeDownload c
  resumeDownload (QBittorrentInstance c) = resumeDownload c

  removeDownload (SABInstance c) = removeDownload c
  removeDownload (TransmissionInstance c) = removeDownload c
  removeDownload (QBittorrentInstance c) = removeDownload c

-- | Create download client instances from configuration
createClientInstances :: HttpClient -> DownloadConfig -> IO [(Text, DownloadClientInstance)]
createClientInstances httpClient DownloadConfig{..} = do
  let clients = catMaybes [downloadNzbClient, downloadTorrentClient]
  catMaybes <$> forM clients (\client -> do
    if not (dcEnabled client)
      then pure Nothing
      else do
        instance' <- case dcType client of
          SABnzbd -> do
            let apiKey = fromMaybe "" (dcApiKey client)
            pure $ Just $ SABInstance $ createSABnzbdClient
              (dcUrl client)
              apiKey
              httpClient
              (dcDownloadDir client)
              (dcCategory client)

          NZBGet -> do
            -- TODO: Implement NZBGet client
            pure Nothing

          Transmission -> do
            client' <- createTransmissionClient
              (dcUrl client)
              (dcUsername client)
              (dcPassword client)
              httpClient
            pure $ Just $ TransmissionInstance client'

          QBittorrent -> do
            let username = fromMaybe "" (dcUsername client)
                password = fromMaybe "" (dcPassword client)
            client' <- createQBittorrentClient
              (dcUrl client)
              username
              password
              httpClient
            pure $ Just $ QBittorrentInstance client'

        pure $ fmap (downloadClientTypeName (dcType client),) instance'
    )

-- | Start the download monitor service
startDownloadMonitor :: ServiceContext -> IO ()
startDownloadMonitor ServiceContext{..} = do
  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar scConfigVar

  runKatipContextT scLogEnv () "download-monitor" $ do
    let bus = scEventBus
        pool = scDbPool
        downloadCfg = download config
        httpClient = scHttpClient

    $(logTM) InfoS "Starting download monitor service"

    clients <- liftIO $ createClientInstances httpClient downloadCfg

    if null clients
      then $(logTM) WarningS "No download clients configured, monitor will not run"
      else do
        -- Spawn monitoring loop in background thread
        _ <- liftIO $ async $ runKatipContextT scLogEnv () "download-monitor" $ forever $ do
          forM_ clients $ \(clientName, client) -> do
            $(logTM) InfoS $ logStr $ "Checking downloads from " <> clientName
            liftIO $ checkAndUpdateDownloads scLogEnv bus pool clientName client

          -- Sleep for the configured check interval (convert seconds to microseconds)
          let intervalSeconds = downloadCheckInterval downloadCfg
          liftIO $ threadDelay (intervalSeconds * 1000000)

        pure ()
  pure ()

-- | Check all downloads for a specific client and update database
checkAndUpdateDownloads :: LogEnv -> EventBus -> ConnectionPool -> Text -> DownloadClientInstance -> IO ()
checkAndUpdateDownloads le bus pool clientName client = do
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
      Nothing -> runKatipContextT le () "download-monitor" $
        $(logTM) WarningS $ logStr $ "Download has no client ID: " <> DB.downloadTitle download
      Just clientId -> do
        statusResult <- getDownloadStatus client clientId

        case statusResult of
          Left err -> runKatipContextT le () "download-monitor" $
            $(logTM) ErrorS $ logStr $ "Failed to get download status for " <> DB.downloadTitle download <> ": " <> err

          Right downloadInfo -> do
            let oldStatus = DB.downloadStatus download
                newStatus = mapClientStatusToDBStatus (DC.diStatus downloadInfo)

            -- Update database with new information
            now <- getCurrentTime
            withConnection pool $ \conn -> do
              executeQuery conn
                "UPDATE downloads SET status = ?, progress = ?, download_path = ?, \
                \error_message = ?, size_bytes = ?, started_at = COALESCE(started_at, ?), \
                \completed_at = ? WHERE id = ?"
                ( downloadStatusToText newStatus
                , DC.diProgress downloadInfo
                , DC.diDownloadPath downloadInfo
                , DC.diErrorMessage downloadInfo
                , DC.diSizeBytes downloadInfo
                , if newStatus == DB.DownloadDownloading then Just now else Nothing
                , if newStatus == DB.DownloadCompleted then Just now else Nothing
                , DB.downloadId download
                )

            -- If status changed to completed, emit event for post-processing
            when (oldStatus /= DB.DownloadCompleted && newStatus == DB.DownloadCompleted) $ do
              runKatipContextT le () "download-monitor" $ do
                $(logTM) InfoS $ logStr $ "Download completed: " <> DB.downloadTitle download
                $(logTM) InfoS $ logStr $ "Download path: " <> fromMaybe "<no path>" (DC.diDownloadPath downloadInfo)
              publishAndLog bus le "download-monitor" $ DownloadCompleted
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadPath = DC.diDownloadPath downloadInfo  -- Use NEW path from client, not old path from DB
                }

            -- If download failed, emit event
            when (oldStatus /= DB.DownloadFailed && newStatus == DB.DownloadFailed) $ do
              runKatipContextT le () "download-monitor" $
                $(logTM) ErrorS $ logStr $ "Download failed: " <> DB.downloadTitle download
              publishAndLog bus le "download-monitor" $ DownloadFailed
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadError = DB.downloadErrorMessage download
                }

-- | Map download client status to database status
mapClientStatusToDBStatus :: DC.DownloadStatus -> DB.DownloadStatus
mapClientStatusToDBStatus DC.DSQueued = DB.DownloadQueued
mapClientStatusToDBStatus DC.DSDownloading = DB.DownloadDownloading
mapClientStatusToDBStatus DC.DSPaused = DB.DownloadQueued  -- Treat paused as queued
mapClientStatusToDBStatus DC.DSCompleted = DB.DownloadCompleted
mapClientStatusToDBStatus DC.DSFailed = DB.DownloadFailed
mapClientStatusToDBStatus DC.DSCancelled = DB.DownloadCancelled
