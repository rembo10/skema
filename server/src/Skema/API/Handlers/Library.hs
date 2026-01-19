{-# LANGUAGE OverloadedStrings #-}

-- | Library API handlers.
module Skema.API.Handlers.Library
  ( libraryServer
  ) where

import Skema.API.Types.Library (LibraryAPI, UpdateTrackRequest(..), TracksResponse(..), TracksPagination(..), TracksStats(..), LibraryTaskRequest(..))
import Skema.API.Types.Tasks (TaskResource(..))
import Skema.Core.TaskManager (TaskManager)
import qualified Skema.Core.TaskManager as TM
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import Database.SQLite.Simple (Only(..))
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Utils (osPathToString, stringToOsPath)
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
import qualified Data.Text as T

-- | Library API handlers.
libraryServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> TaskManager -> ConnectionPool -> TVar Cfg.Config -> Server LibraryAPI
libraryServer le bus _serverCfg jwtSecret _registry tm pool configVar = \maybeAuthHeader ->
  taskHandler maybeAuthHeader
  :<|> filesHandler maybeAuthHeader
  :<|> tracksHandler maybeAuthHeader
  :<|> tracksStatsHandler maybeAuthHeader
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
                libPathText <- osPathToString libOsPath
                EventBus.publishAndLog bus le "library-task" $
                  Events.LibraryScanRequested
                    { Events.scanPath = toText libPathText
                    , Events.forceRescan = False
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

    tracksHandler :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler TracksResponse
    tracksHandler authHeader maybeOffset maybeLimit maybeFilter maybeSort maybeOrder maybeSearch = do
      _ <- requireAuth configVar jwtSecret authHeader
      let offset = fromMaybe 0 maybeOffset
      let limit = fromMaybe 50 maybeLimit
      let filterStatus = fromMaybe "all" maybeFilter
      let sortField = fromMaybe "album" maybeSort
      let sortOrder = fromMaybe "asc" maybeOrder
      let searchQuery = maybeSearch

      liftIO $ withConnection pool $ \conn -> do
        -- Build WHERE clause based on filter
        let filterClause = case filterStatus of
              "matched" -> "c.mb_release_id IS NOT NULL" :: Text
              "unmatched" -> "c.mb_release_id IS NULL" :: Text
              "locked" -> "c.match_locked = 1" :: Text
              _ -> "" :: Text

        -- Build search clause
        let searchClause = case searchQuery of
              Nothing -> "" :: Text
              Just q ->
                let pattern = "%" <> q <> "%"
                in "(m.title LIKE " <> show pattern <> " OR " <>
                   "m.artist LIKE " <> show pattern <> " OR " <>
                   "c.album LIKE " <> show pattern <> " OR " <>
                   "c.album_artist LIKE " <> show pattern <> " OR " <>
                   "t.mb_recording_title LIKE " <> show pattern <> " OR " <>
                   "t.path LIKE " <> show pattern <> ")" :: Text

        -- Combine clauses
        let whereClauses = filter (/= "") [filterClause, searchClause]
        let whereClause = if null whereClauses
                         then "" :: Text
                         else "WHERE " <> T.intercalate " AND " whereClauses

        -- Build ORDER BY clause based on sort field
        let orderByClause = case sortField of
              "album" -> "c.album"
              "artist" -> "m.artist"
              "track_title" -> "m.title"
              "confidence" -> "c.mb_confidence"
              "status" -> "CASE WHEN c.mb_release_id IS NOT NULL THEN 1 ELSE 0 END"
              _ -> "c.album"  -- default to album

        let direction = if sortOrder == "desc" then "DESC" else "ASC"
        let orderBy = orderByClause <> " " <> direction <> ", m.disc_number, m.track_number"

        -- Get total count with filter and search
        [Only totalCount] <- queryRows conn
          ("SELECT COUNT(*) FROM library_tracks t \
          \LEFT JOIN library_track_metadata m ON t.id = m.track_id \
          \LEFT JOIN clusters c ON t.cluster_id = c.id " <> whereClause)
          ()

        -- Get paginated tracks with filter and sort
        tracks <- queryRows conn
          ("SELECT \
          \  t.id, t.path, m.title, m.artist, m.track_number, m.disc_number, m.duration_seconds, \
          \  t.mb_recording_id, t.mb_recording_title, \
          \  t.cluster_id, c.album, c.album_artist, m.year, \
          \  c.mb_release_id, \
          \  json_extract(c.mb_release_data, '$.title'), \
          \  json_extract(c.mb_release_data, '$.artist-credit[0].name'), \
          \  c.mb_confidence, c.match_source, COALESCE(c.match_locked, 0) \
          \FROM library_tracks t \
          \LEFT JOIN library_track_metadata m ON t.id = m.track_id \
          \LEFT JOIN clusters c ON t.cluster_id = c.id " <>
          whereClause <>
          " ORDER BY " <> orderBy <>
          " LIMIT ? OFFSET ?")
          (limit, offset)

        pure $ TracksResponse
          { tracksResponsePagination = TracksPagination
              { tracksPaginationTotal = totalCount
              , tracksPaginationOffset = offset
              , tracksPaginationLimit = limit
              }
          , tracksResponseTracks = tracks
          }

    tracksStatsHandler :: Maybe Text -> Handler TracksStats
    tracksStatsHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader

      liftIO $ withConnection pool $ \conn -> do
        -- Get total count
        [Only total] <- queryRows conn
          "SELECT COUNT(*) FROM library_tracks"
          ()

        -- Get matched count (tracks with mb_release_id via cluster)
        [Only matched] <- queryRows conn
          "SELECT COUNT(*) FROM library_tracks t \
          \LEFT JOIN clusters c ON t.cluster_id = c.id \
          \WHERE c.mb_release_id IS NOT NULL"
          ()

        -- Get locked count
        [Only locked] <- queryRows conn
          "SELECT COUNT(*) FROM library_tracks t \
          \LEFT JOIN clusters c ON t.cluster_id = c.id \
          \WHERE c.match_locked = 1"
          ()

        let unmatched = total - matched

        pure $ TracksStats
          { tracksStatsTotal = total
          , tracksStatsMatched = matched
          , tracksStatsUnmatched = unmatched
          , tracksStatsLocked = locked
          }

    updateTrackHandler :: Maybe Text -> Int64 -> UpdateTrackRequest -> Handler NoContent
    updateTrackHandler authHeader trackId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection pool $ \conn ->
        executeQuery conn
          "UPDATE library_tracks SET cluster_id = ? WHERE id = ?"
          (updateTrackClusterId req, trackId)
      pure NoContent
