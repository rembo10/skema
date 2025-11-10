{-# LANGUAGE OverloadedStrings #-}

-- | Downloads API handlers.
module Skema.API.Handlers.Downloads
  ( downloadsServer
  ) where

import Skema.API.Types.Downloads (DownloadsAPI, DownloadResponse(..), QueueDownloadRequest, QueueDownloadResponse(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import Skema.Domain.Converters (downloadRecordToResponse)
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Config.Types as Cfg
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Servant
import Katip
import Database.SQLite.Simple (Only(..))
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Downloads API handlers.
downloadsServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> STM.TVar (Map.Map Int64 (Double, Text)) -> TVar Cfg.Config -> Server DownloadsAPI
downloadsServer le bus _serverCfg jwtSecret connPool progressMap configVar = \maybeAuthHeader ->
  getAllDownloadsHandler maybeAuthHeader
  :<|> getDownloadHandler maybeAuthHeader
  :<|> queueDownloadHandler maybeAuthHeader
  :<|> pauseDownloadHandler maybeAuthHeader
  :<|> resumeDownloadHandler maybeAuthHeader
  :<|> deleteDownloadHandler maybeAuthHeader
  :<|> reidentifyDownloadHandler maybeAuthHeader
  where
    getAllDownloadsHandler :: Maybe Text -> Handler [DownloadResponse]
    getAllDownloadsHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        downloads <- queryRows conn
          "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
          \download_client_id, status, download_path, title, size_bytes, quality, \
          \format, seeders, progress, error_message, queued_at, started_at, \
          \completed_at, imported_at, updated_at, matched_cluster_id, library_path \
          \FROM downloads ORDER BY queued_at DESC"
          () :: IO [DBTypes.DownloadRecord]
        -- Read current progress and status from memory
        progressMapData <- STM.atomically $ STM.readTVar progressMap
        -- Merge in-memory progress and display status with database records
        forM downloads $ \download -> do
          let downloadId = fromMaybe 0 (DBTypes.downloadId download)
              -- Use in-memory data if available, otherwise use database values
              (currentProgress, maybeDisplayStatus) = case Map.lookup downloadId progressMapData of
                Just (prog, status) -> (prog, Just status)
                Nothing -> (DBTypes.downloadProgress download, Nothing)
              -- Create updated record with current progress
              updatedDownload = download { DBTypes.downloadProgress = currentProgress }
              -- Create response with display status override if available
              response = downloadRecordToResponse updatedDownload
          pure $ case maybeDisplayStatus of
            Just displayStatus -> response { downloadResponseStatus = displayStatus }
            Nothing -> response

    getDownloadHandler :: Maybe Text -> Int64 -> Handler DownloadResponse
    getDownloadHandler authHeader downloadId = do
      _ <- requireAuth configVar jwtSecret authHeader
      downloads <- liftIO $ withConnection connPool $ \conn ->
        queryRows conn
          "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
          \download_client_id, status, download_path, title, size_bytes, quality, \
          \format, seeders, progress, error_message, queued_at, started_at, \
          \completed_at, imported_at, updated_at, matched_cluster_id, library_path \
          \FROM downloads WHERE id = ?"
          (Only downloadId) :: IO [DBTypes.DownloadRecord]
      case downloads of
        [] -> throw404 $ "Download not found: " <> show downloadId
        (download:_) -> do
          -- Read current progress and status from memory
          progressMapData <- liftIO $ STM.atomically $ STM.readTVar progressMap
          -- Use in-memory data if available, otherwise use database values
          let (currentProgress, maybeDisplayStatus) = case Map.lookup downloadId progressMapData of
                Just (prog, status) -> (prog, Just status)
                Nothing -> (DBTypes.downloadProgress download, Nothing)
              updatedDownload = download { DBTypes.downloadProgress = currentProgress }
              response = downloadRecordToResponse updatedDownload
          pure $ case maybeDisplayStatus of
            Just displayStatus -> response { downloadResponseStatus = displayStatus }
            Nothing -> response

    queueDownloadHandler :: Maybe Text -> QueueDownloadRequest -> Handler QueueDownloadResponse
    queueDownloadHandler authHeader _req = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- TODO: Implement download queueing
      -- This requires:
      -- 1. Select appropriate download client from config
      -- 2. Call download client's addDownload method
      -- 3. Store download record in database
      -- 4. Emit DownloadQueued event
      throw500 "Download queueing not yet implemented"

    pauseDownloadHandler :: Maybe Text -> Int64 -> Handler NoContent
    pauseDownloadHandler authHeader _downloadId = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- TODO: Implement pause
      -- Get download client info and call pauseDownload
      throw500 "Pause not yet implemented"

    resumeDownloadHandler :: Maybe Text -> Int64 -> Handler NoContent
    resumeDownloadHandler authHeader _downloadId = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- TODO: Implement resume
      -- Get download client info and call resumeDownload
      throw500 "Resume not yet implemented"

    deleteDownloadHandler :: Maybe Text -> Int64 -> Maybe Bool -> Handler NoContent
    deleteDownloadHandler authHeader _downloadId _deleteFiles = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- TODO: Implement delete
      -- Get download client info and call removeDownload
      throw500 "Delete not yet implemented"

    reidentifyDownloadHandler :: Maybe Text -> Int64 -> Handler NoContent
    reidentifyDownloadHandler authHeader downloadId = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- Emit DownloadCompleted event to trigger re-import/re-identification
      liftIO $ EventBus.publishAndLog bus le "api.downloads" $ Events.DownloadCompleted
        { Events.downloadId = downloadId
        , Events.downloadTitle = ""  -- Will be looked up by importer
        , Events.downloadPath = Nothing  -- Will be looked up by importer
        }
      pure NoContent
