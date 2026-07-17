{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Downloads API handlers.
module Skema.API.Handlers.Downloads
  ( downloadsServer
  ) where

import Skema.API.Types.Downloads (DownloadsAPI, DownloadResponse(..), DownloadsPagination(..), DownloadsResponse(..), QueueDownloadRequest(..), SlskdFileRequest(..), QueueDownloadResponse(..))
import Skema.API.Types.Tasks (TaskRequest(..), TaskResponse(..), TaskResource(..))
import qualified Skema.Services.TaskManager as TM
import Skema.API.Handlers.Utils (throw400, throw404, readConfig, parsePagination)
import Skema.Database.Connection
import Skema.Domain.Converters (downloadRecordToResponse)
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Repository.Downloads as DownloadsRepo
import qualified Skema.Config.Types as Cfg
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Skema.Services.Download.Submission (submitDownload, DownloadSubmissionContext(..))
import Skema.Services.Download.Cancel (cancelDownload)
import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..), SlskdFile(..))
import Skema.Domain.Quality (textToQuality)
import Control.Concurrent.Async (async)
import Data.Aeson (toJSON, object, (.=))
import Skema.Clock (getNow)
import Servant
import Skema.API.App (Env(..), AppM)
import Database.SQLite.Simple (Only(..))
import qualified Data.Map.Strict as Map
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T

-- | Downloads API handlers.
downloadsServer :: Env -> ServerT DownloadsAPI AppM
downloadsServer Env{envLogEnv = le, envEventBus = bus, envTaskManager = tm, envDbPool = connPool, envProgressMap = progressMap, envConfigVar = configVar, envHttpClient = httpClient, envClock = clock} =
  taskHandler
  :<|> getAllDownloadsHandler
  :<|> getDownloadHandler
  :<|> queueDownloadHandler
  :<|> deleteDownloadHandler
  where
    taskHandler :: TaskRequest -> AppM TaskResponse
    taskHandler req = do
      let downloadId = fromMaybe 0 (taskRequestResourceId req)

      -- Create task based on request type
      case taskRequestType req of
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
            maybeDownload <- withConnection connPool $ \conn ->
              DownloadsRepo.getDownloadById conn downloadId

            case maybeDownload of
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

    getAllDownloadsHandler :: Maybe Int -> Maybe Int -> Maybe Int64 -> AppM DownloadsResponse
    getAllDownloadsHandler maybeOffset maybeLimit maybeCatalogAlbumId = do
      let (offset, limit) = parsePagination maybeOffset maybeLimit

      (allDownloads, responses) <- liftIO $ withConnection connPool $ \conn -> do
        allDownloads' <- DownloadsRepo.getAllDownloads conn
        -- Optionally scope to a single catalog album (for the album detail page)
        let allDownloads = case maybeCatalogAlbumId of
              Just albumId -> filter (\d -> DBTypes.downloadCatalogAlbumId d == albumId) allDownloads'
              Nothing -> allDownloads'

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

    getDownloadHandler :: Int64 -> AppM DownloadResponse
    getDownloadHandler downloadId = do
      maybeDownload <- liftIO $ withConnection connPool $ \conn ->
        DownloadsRepo.getDownloadById conn downloadId
      case maybeDownload of
        Nothing -> throw404 $ "Download not found: " <> show downloadId
        Just download -> do
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

    queueDownloadHandler :: QueueDownloadRequest -> AppM QueueDownloadResponse
    queueDownloadHandler req = do
      -- Read current config from TVar
      config <- liftIO $ readConfig configVar

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
      quality <- case qualityText of
        Nothing -> throw400 "Quality is required"
        Just qt -> case textToQuality qt of
          Nothing -> throw400 $ "Invalid quality: " <> qt
          Just q -> pure q

      -- Determine download type from format
      downloadType <- case format of
        Just "NZB" -> pure NZB
        Just "TORRENT" -> pure Torrent
        Just "SLSKD" -> pure Slskd
        _ -> throw400 $ "Invalid download format: " <> fromMaybe "(missing)" format

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
      now <- liftIO (getNow (clock))
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
            , dscHttpClient = httpClient
            , dscDownloadConfig = Cfg.download config
            , dscIndexerName = indexerName
            , dscClock = clock
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

    deleteDownloadHandler :: Int64 -> AppM NoContent
    deleteDownloadHandler downloadId = do
      config <- liftIO $ readConfig configVar
      liftIO $ cancelDownload le (httpClient) (Cfg.download config) connPool downloadId
      pure NoContent
