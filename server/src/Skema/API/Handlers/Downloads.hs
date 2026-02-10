{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Downloads API handlers.
module Skema.API.Handlers.Downloads
  ( downloadsServer
  ) where

import Skema.API.Types.Downloads (DownloadsAPI, DownloadResponse(..), DownloadsPagination(..), DownloadsResponse(..), QueueDownloadRequest(..), SlskdFileRequest(..), QueueDownloadResponse(..), DownloadTaskRequest(..))
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
import Skema.Services.Download.Client (createClientInstance)
import Skema.DownloadClient.Types (removeDownload)
import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..), SlskdFile(..))
import qualified Skema.Slskd.Client as Slskd
import qualified Skema.Slskd.Types as SlskdTypes
import Skema.Domain.Quality (textToQuality)
import Control.Concurrent.Async (async)
import Data.Aeson (toJSON, object, (.=))
import Data.Time (getCurrentTime)
import Servant
import Katip
import Database.SQLite.Simple (Only(..))
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

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

        "retry" -> liftIO $ do
          -- Create the task
          taskResp <- TM.createTask tm DownloadsResource (Just downloadId) "retry"
          let taskId = taskResponseId taskResp

          -- Spawn async worker to retry the download
          _ <- async $ do
            TM.updateTaskProgress tm taskId 0.2 (Just "Looking up download...")

            -- Get the download record to determine what stage it failed at
            downloads <- withConnection connPool $ \conn ->
              queryRows conn
                "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
                \download_client_id, status, download_path, title, size_bytes, quality, \
                \format, seeders, progress, error_message, queued_at, started_at, \
                \completed_at, imported_at, updated_at, \
                \matched_cluster_id, library_path \
                \FROM downloads WHERE id = ?"
                (Only downloadId) :: IO [DBTypes.DownloadRecord]

            case viaNonEmpty head downloads of
              Nothing -> do
                TM.failTask tm taskId "Download not found"
              Just download -> do
                let status = DBTypes.downloadStatus download
                    hasPath = isJust (DBTypes.downloadPath download) && not (T.null $ T.strip $ fromMaybe "" $ DBTypes.downloadPath download)

                TM.updateTaskProgress tm taskId 0.5 (Just "Retrying download...")

                case status of
                  -- If download failed during download phase, reset to queued
                  DBTypes.DownloadFailed | not hasPath -> do
                    -- Reset download to queued state
                    withConnection connPool $ \conn ->
                      executeQuery conn
                        "UPDATE downloads SET status = ?, error_message = NULL, progress = 0 WHERE id = ?"
                        ("queued" :: Text, downloadId)
                    TM.completeTask tm taskId (Just $ toJSON $ object
                      [ "message" .= ("Download reset to queued - will be picked up by download monitor" :: Text)
                      , "download_id" .= downloadId
                      ])

                  -- If download completed but import failed, retry import
                  DBTypes.DownloadFailed | hasPath -> do
                    -- Clear error and re-emit DownloadCompleted event
                    withConnection connPool $ \conn ->
                      executeQuery conn
                        "UPDATE downloads SET error_message = NULL WHERE id = ?"
                        (Only downloadId)
                    EventBus.publishAndLog bus le "api.downloads.task" $ Events.DownloadCompleted
                      { Events.downloadId = downloadId
                      , Events.downloadTitle = DBTypes.downloadTitle download
                      , Events.downloadPath = DBTypes.downloadPath download
                      }
                    TM.completeTask tm taskId (Just $ toJSON $ object
                      [ "message" .= ("Import retry requested" :: Text)
                      , "download_id" .= downloadId
                      ])

                  -- For identification failures, use reidentify instead
                  DBTypes.DownloadIdentificationFailure -> do
                    withConnection connPool $ \conn ->
                      executeQuery conn
                        "UPDATE downloads SET error_message = NULL WHERE id = ?"
                        (Only downloadId)
                    EventBus.publishAndLog bus le "api.downloads.task" $ Events.DownloadCompleted
                      { Events.downloadId = downloadId
                      , Events.downloadTitle = DBTypes.downloadTitle download
                      , Events.downloadPath = DBTypes.downloadPath download
                      }
                    TM.completeTask tm taskId (Just $ toJSON $ object
                      [ "message" .= ("Retrying identification" :: Text)
                      , "download_id" .= downloadId
                      ])

                  _ -> do
                    TM.failTask tm taskId $ "Cannot retry download with status: " <> show status

          pure taskResp

        _ -> throwError err400 { errBody = "Unknown task type" }

    getAllDownloadsHandler :: Maybe Text -> Maybe Int -> Maybe Int -> Handler DownloadsResponse
    getAllDownloadsHandler authHeader maybeOffset maybeLimit = do
      _ <- requireAuth configVar jwtSecret authHeader

      let offset = fromMaybe 0 maybeOffset
      let limit = fromMaybe 50 maybeLimit

      (allDownloads, responses) <- liftIO $ withConnection connPool $ \conn -> do
        allDownloads <- queryRows conn
          "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
          \download_client_id, status, download_path, title, size_bytes, quality, \
          \format, seeders, progress, error_message, queued_at, started_at, \
          \completed_at, imported_at, updated_at, matched_cluster_id, library_path \
          \FROM downloads ORDER BY queued_at DESC"
          () :: IO [DBTypes.DownloadRecord]

        let paginated = take limit $ drop offset $ allDownloads

        -- Read current progress and status from memory
        progressMapData <- STM.atomically $ STM.readTVar progressMap
        -- Merge in-memory progress and display status with database records
        responses <- forM paginated $ \download -> do
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

        pure (allDownloads, responses)

      let total = length allDownloads
      let pagination = DownloadsPagination
            { downloadsPaginationTotal = total
            , downloadsPaginationOffset = offset
            , downloadsPaginationLimit = limit
            }

      pure DownloadsResponse
        { downloadsResponsePagination = pagination
        , downloadsResponseDownloads = responses
        }

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
          slskdUsername = queueDownloadSlskdUsername req
          slskdFiles = queueDownloadSlskdFiles req

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
            Just "SLSKD" -> Slskd
            _ -> error $ "Invalid download format: " <> show format

      -- Convert slskd files if present
      let convertSlskdFile f = SlskdFile
            { sfFilename = slskdFileRequestFilename f
            , sfSize = slskdFileRequestSize f
            , sfBitRate = Nothing
            , sfSampleRate = Nothing
            , sfBitDepth = Nothing
            , sfLength = Nothing
            , sfIsLocked = False
            }

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
            , riSlskdUsername = slskdUsername
            , riSlskdFiles = fmap (map convertSlskdFile) slskdFiles
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

      -- Look up the download to check if it's in progress
      downloads <- liftIO $ withConnection connPool $ \conn ->
        queryRows conn
          "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
          \download_client_id, status, download_path, title, size_bytes, quality, \
          \format, seeders, progress, error_message, queued_at, started_at, \
          \completed_at, imported_at, updated_at, \
          \matched_cluster_id, library_path \
          \FROM downloads WHERE id = ?"
          (Only downloadId) :: IO [DBTypes.DownloadRecord]

      -- If download is in progress, try to cancel it from the client
      case downloads of
        (download:_) -> do
          let status = DBTypes.downloadStatus download
              clientName = DBTypes.downloadClient download
              clientId = DBTypes.downloadClientId download

          -- Cancel if downloading or queued
          when (status == DBTypes.DownloadDownloading || status == DBTypes.DownloadQueued) $ do
            config <- liftIO $ STM.atomically $ STM.readTVar configVar
            liftIO $ runKatipContextT le () "api.downloads.delete" $ do
              $(logTM) InfoS $ logStr $ ("Cancelling in-progress download " <> show downloadId <> " from client: " <> show clientName :: Text)

            case clientName of
              Just "slskd" -> do
                -- For slskd, we need to cancel transfers
                -- The client_id format is "username:title", extract username
                case clientId of
                  Just cid -> do
                    let username = T.takeWhile (/= ':') cid
                    -- Get slskd config and cancel transfers
                    let maybeSlskdConfig = Cfg.downloadSlskdClient (Cfg.download config)
                    case maybeSlskdConfig of
                      Just slskdConfig | Cfg.slskdEnabled slskdConfig -> do
                        let httpClient = srHttpClient registry
                        let client = Slskd.createSlskdClient slskdConfig httpClient
                        -- Get transfers for this user and cancel them
                        result <- liftIO $ Slskd.getTransfersByUsername client username
                        case result of
                          Left err -> liftIO $ runKatipContextT le () "api.downloads.delete" $
                            $(logTM) WarningS $ logStr $ ("Failed to get slskd transfers: " <> err :: Text)
                          Right transfers -> do
                            -- Cancel each transfer
                            forM_ transfers $ \transfer -> do
                              _ <- liftIO $ Slskd.cancelTransfer client username (SlskdTypes.stId transfer) True
                              pure ()
                            liftIO $ runKatipContextT le () "api.downloads.delete" $
                              $(logTM) InfoS $ logStr $ ("Cancelled " <> show (length transfers) <> " slskd transfers" :: Text)
                      _ -> pure ()
                  Nothing -> pure ()

              Just client | client == "SABnzbd" || client == "NZBGet" -> do
                -- For NZB clients, use removeDownload
                case clientId of
                  Just cid -> do
                    let nzbConfig = Cfg.downloadNzbClient (Cfg.download config)
                    case nzbConfig of
                      Just nzbClient | Cfg.dcEnabled nzbClient -> do
                        let httpClient = srHttpClient registry
                        clientInstance <- liftIO $ createClientInstance httpClient nzbClient
                        result <- liftIO $ removeDownload clientInstance cid True
                        case result of
                          Left err -> liftIO $ runKatipContextT le () "api.downloads.delete" $
                            $(logTM) WarningS $ logStr $ ("Failed to remove from NZB client: " <> err :: Text)
                          Right () -> liftIO $ runKatipContextT le () "api.downloads.delete" $
                            $(logTM) InfoS $ logStr $ ("Removed download from NZB client" :: Text)
                      _ -> pure ()
                  Nothing -> pure ()

              Just client | client == "Transmission" || client == "qBittorrent" -> do
                -- For torrent clients, use removeDownload
                case clientId of
                  Just cid -> do
                    let torrentConfig = Cfg.downloadTorrentClient (Cfg.download config)
                    case torrentConfig of
                      Just torrentClient | Cfg.dcEnabled torrentClient -> do
                        let httpClient = srHttpClient registry
                        clientInstance <- liftIO $ createClientInstance httpClient torrentClient
                        result <- liftIO $ removeDownload clientInstance cid True
                        case result of
                          Left err -> liftIO $ runKatipContextT le () "api.downloads.delete" $
                            $(logTM) WarningS $ logStr $ ("Failed to remove from torrent client: " <> err :: Text)
                          Right () -> liftIO $ runKatipContextT le () "api.downloads.delete" $
                            $(logTM) InfoS $ logStr $ ("Removed download from torrent client" :: Text)
                      _ -> pure ()
                  Nothing -> pure ()

              _ -> pure ()  -- Unknown client, just delete from DB

        [] -> pure ()  -- Download not found, just proceed with delete

      -- Delete the download record from the database
      liftIO $ withConnection connPool $ \conn ->
        DownloadsRepo.deleteDownload conn downloadId
      pure NoContent
