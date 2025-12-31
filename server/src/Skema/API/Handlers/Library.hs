{-# LANGUAGE OverloadedStrings #-}

-- | Library API handlers.
module Skema.API.Handlers.Library
  ( libraryServer
  ) where

import Skema.API.Types.Library (LibraryAPI, UpdateTrackRequest(..), TrackWithCluster(..), LibraryTaskRequest(..))
import Skema.API.Types.Tasks (TaskResource(..))
import Skema.Core.TaskManager (TaskManager)
import qualified Skema.Core.TaskManager as TM
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Config.Types as Cfg
import Skema.Services.Registry (ServiceRegistry)
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import qualified System.OsPath as OP
import Servant
import Katip
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async (async)
import Data.Aeson (toJSON, object, (.=))
import Skema.API.Types.Tasks (TaskResponse(..))

-- | Library API handlers.
libraryServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> TaskManager -> ConnectionPool -> TVar Cfg.Config -> Server LibraryAPI
libraryServer le bus _serverCfg jwtSecret _registry tm pool configVar = \maybeAuthHeader ->
  taskHandler maybeAuthHeader
  :<|> filesHandler maybeAuthHeader
  :<|> tracksHandler maybeAuthHeader
  :<|> updateTrackHandler maybeAuthHeader
  where
    taskHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      config <- liftIO $ STM.atomically $ STM.readTVar configVar

      -- Create task based on request type
      case libraryTaskType req of
        "scan" -> liftIO $ do
          -- Create the task
          taskResp <- TM.createTask tm LibraryResource Nothing "scan"
          let taskId = taskResponseId taskResp

          -- Spawn async worker to execute the scan
          _ <- async $ do
            -- Emit library scan requested event
            case Cfg.libraryPath (Cfg.library config) of
              Nothing -> do
                TM.failTask tm taskId "Library path not configured"
              Just libOsPath -> do
                libPathText <- OP.decodeUtf libOsPath
                EventBus.publishAndLog bus le "library-task" $
                  Events.LibraryScanRequested
                    { Events.scanPath = toText libPathText
                    }

                -- For now, just complete the task immediately
                -- TODO: Hook up actual scan logic with progress updates
                TM.updateTaskProgress tm taskId 0.5 (Just "Scanning files...")
                TM.completeTask tm taskId (Just $ toJSON $ object
                  [ "message" .= ("Scan completed for: " <> toText libPathText :: Text)
                  , "files_scanned" .= (0 :: Int)
                  ])

          pure taskResp
        _ -> throwError err400 { errBody = "Unknown task type" }

    filesHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- TODO: Fetch from database
      pure []

    tracksHandler :: Maybe Text -> Handler [TrackWithCluster]
    tracksHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection pool $ \conn ->
        queryRows conn
          "SELECT \
          \  t.id, t.path, m.title, m.artist, m.track_number, m.disc_number, m.duration_seconds, \
          \  t.mb_recording_id, t.mb_recording_title, \
          \  t.cluster_id, c.album, c.album_artist, m.year, \
          \  c.mb_release_id, \
          \  json_extract(c.mb_release_data, '$.title'), \
          \  json_extract(c.mb_release_data, '$.artist-credit[0].name'), \
          \  c.mb_confidence, c.match_source, COALESCE(c.match_locked, 0) \
          \FROM library_tracks t \
          \LEFT JOIN library_track_metadata m ON t.id = m.track_id \
          \LEFT JOIN clusters c ON t.cluster_id = c.id \
          \ORDER BY c.album, m.disc_number, m.track_number"
          ()

    updateTrackHandler :: Maybe Text -> Int64 -> UpdateTrackRequest -> Handler NoContent
    updateTrackHandler authHeader trackId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection pool $ \conn ->
        executeQuery conn
          "UPDATE library_tracks SET cluster_id = ? WHERE id = ?"
          (updateTrackClusterId req, trackId)
      pure NoContent
