{-# LANGUAGE OverloadedStrings #-}

-- | Downloads API handlers.
module Skema.API.Handlers.Downloads
  ( downloadsServer
  ) where

import Skema.API.Types.Downloads (DownloadsAPI, DownloadResponse(..), QueueDownloadRequest(..), QueueDownloadResponse(..), DownloadTaskRequest(..))
import Skema.API.Types.Tasks (TaskResponse(..), TaskResource(..))
import Skema.Core.TaskManager (TaskManager)
import qualified Skema.Core.TaskManager as TM
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import Skema.Domain.Converters (downloadRecordToResponse)
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Repository.Downloads as DownloadsRepo
import qualified Skema.Config.Types as Cfg
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.Services.Download.Submission (submitDownload, DownloadSubmissionContext(..))
import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..))
import Skema.Domain.Quality (textToQuality)
import Control.Concurrent.Async (async)
import Data.Aeson (toJSON, object, (.=))
import Data.Time (getCurrentTime)
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
downloadsServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> TaskManager -> ConnectionPool -> STM.TVar (Map.Map Int64 (Double, Text)) -> TVar Cfg.Config -> Server DownloadsAPI
downloadsServer le bus _serverCfg jwtSecret registry tm connPool progressMap configVar = \maybeAuthHeader ->
  taskHandler maybeAuthHeader
  :<|> getAllDownloadsHandler maybeAuthHeader
  :<|> getDownloadHandler maybeAuthHeader
  :<|> queueDownloadHandler maybeAuthHeader
  :<|> deleteDownloadHandler maybeAuthHeader
  where
    taskHandler :: Maybe Text -> DownloadTaskRequest -> Handler TaskResponse
    taskHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      let downloadId = downloadTaskDownloadId req

      -- Create task based on request type
      case downloadTaskType req of
        "reidentify" -> liftIO $ do
          -- Create the task
          taskResp <- TM.createTask tm DownloadsResource (Just downloadId) "reidentify"
          let taskId = taskResponseId taskResp

          -- Spawn async worker to execute the reidentification
          _ <- async $ do
            TM.updateTaskProgress tm taskId 0.3 (Just "Emitting reidentify event...")
            -- Emit DownloadCompleted event to trigger re-import/re-identification
            EventBus.publishAndLog bus le "api.downloads.task" $ Events.DownloadCompleted
              { Events.downloadId = downloadId
              , Events.downloadTitle = ""  -- Will be looked up by importer
              , Events.downloadPath = Nothing  -- Will be looked up by importer
              }
            TM.completeTask tm taskId (Just $ toJSON $ object
              [ "message" .= ("Download reidentification requested" :: Text)
              , "download_id" .= downloadId
              ])

          pure taskResp

        _ -> throwError err400 { errBody = "Unknown task type" }

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
    queueDownloadHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Read current config from TVar
      config <- liftIO $ STM.atomically $ STM.readTVar configVar

      let catalogAlbumId = queueDownloadCatalogAlbumId req
          indexerName = queueDownloadIndexerName req
          downloadUrl = queueDownloadUrl req
          title = queueDownloadTitle req
          sizeBytes = queueDownloadSizeBytes req
          qualityText = queueDownloadQuality req
          format = queueDownloadFormat req
          seeders = queueDownloadSeeders req

      -- Parse quality from text
      let quality = case qualityText of
            Just qt -> case textToQuality qt of
              Just q -> q
              Nothing -> error $ "Invalid quality: " <> show qt
            Nothing -> error "Quality is required"

      -- Determine download type from format
      let downloadType = case format of
            Just "NZB" -> NZB
            Just "TORRENT" -> Torrent
            _ -> error $ "Invalid download format: " <> show format

      -- Create ReleaseInfo from request
      now <- liftIO getCurrentTime
      let release = ReleaseInfo
            { riTitle = title
            , riGuid = Nothing
            , riDownloadUrl = downloadUrl
            , riInfoUrl = Nothing
            , riSize = fmap fromIntegral sizeBytes
            , riPublishDate = Just now
            , riCategory = Nothing
            , riSeeders = seeders
            , riPeers = Nothing
            , riGrabs = Nothing
            , riDownloadType = downloadType
            , riQuality = quality
            }

      -- Create submission context
      let submissionCtx = DownloadSubmissionContext
            { dscEventBus = bus
            , dscLogEnv = le
            , dscDbPool = connPool
            , dscHttpClient = srHttpClient registry
            , dscDownloadConfig = Cfg.download config
            , dscIndexerName = indexerName
            }

      -- Submit download
      maybeDownloadId <- liftIO $ submitDownload submissionCtx release catalogAlbumId

      case maybeDownloadId of
        Just downloadId -> pure $ QueueDownloadResponse
          { queueDownloadResponseId = downloadId
          , queueDownloadResponseSuccess = True
          , queueDownloadResponseMessage = Just "Download queued successfully"
          }
        Nothing -> pure $ QueueDownloadResponse
          { queueDownloadResponseId = 0
          , queueDownloadResponseSuccess = False
          , queueDownloadResponseMessage = Just "Failed to queue download - check logs for details"
          }

    deleteDownloadHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteDownloadHandler authHeader downloadId = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- Delete the download record from the database
      liftIO $ withConnection connPool $ \conn ->
        DownloadsRepo.deleteDownload conn downloadId
      pure NoContent
