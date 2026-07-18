{-# LANGUAGE OverloadedStrings #-}

-- | Library API handlers.
module Skema.API.Handlers.Library
  ( libraryServer
  ) where

import Skema.API.Types.Library (LibraryAPI, UpdateTrackRequest(..), TracksResponse(..), TracksPagination(..), TracksStats(..), FileInfo(..))
import Skema.API.Types.Tasks (TaskRequest(..), TaskResource(..))
import qualified Skema.Services.TaskManager as TM
import Skema.API.Handlers.Utils (throw400, readConfig, parsePagination)
import Skema.Database.Connection
import Skema.Database.Repository (TracksQuery(..), searchLibraryTracksPage, getLibraryTrackStats, setTrackClusterAssignment)
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Utils (osPathToString)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Servant
import Skema.API.App (Env(..), AppM)
import Data.Aeson (toJSON, object, (.=))
import Skema.API.Types.Tasks (TaskResponse(..))

-- | Library API handlers.
libraryServer :: Env -> ServerT LibraryAPI AppM
libraryServer Env{envLogEnv = le, envEventBus = bus, envTaskManager = tm, envDbPool = pool, envConfigVar = configVar} =
  taskHandler
  :<|> filesHandler
  :<|> tracksHandler
  :<|> tracksStatsHandler
  :<|> updateTrackHandler
  where
    taskHandler :: TaskRequest -> AppM TaskResponse
    taskHandler req = do
      config <- readConfig configVar
      case taskRequestType req of
        "scan" -> TM.startTask tm LibraryResource Nothing "scan" $ \taskId ->
          -- Emit library scan requested event
          case Cfg.libraryPath (Cfg.library config) of
            Nothing ->
              TM.failTask tm taskId "Library path not configured"
            Just libOsPath -> do
              libPathText <- osPathToString libOsPath
              EventBus.publishAndLog bus le "library-task" $
                Events.LibraryScanRequested
                  { Events.scanPath = toText libPathText
                  , Events.forceRescan = False
                  }
              TM.completeTask tm taskId (Just $ toJSON $ object
                [ "message" .= ("Scan requested for: " <> toText libPathText :: Text)
                ])
        _ -> throw400 "Unknown task type"

    filesHandler :: AppM [FileInfo]
    filesHandler =
      -- TODO: Fetch from database
      pure []

    tracksHandler :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppM TracksResponse
    tracksHandler maybeOffset maybeLimit maybeFilter maybeSort maybeOrder maybeSearch = do
      let (offset, limit) = parsePagination maybeOffset maybeLimit
      let query = TracksQuery
            { tqOffset = offset
            , tqLimit = limit
            , tqFilter = fromMaybe "all" maybeFilter
            , tqSort = fromMaybe "album" maybeSort
            , tqOrder = fromMaybe "asc" maybeOrder
            , tqSearch = maybeSearch
            }

      (totalCount, tracks) <- liftIO $ withConnection pool $ \conn ->
        searchLibraryTracksPage conn query

      pure $ TracksResponse
        { tracksResponsePagination = TracksPagination
            { tracksPaginationTotal = totalCount
            , tracksPaginationOffset = offset
            , tracksPaginationLimit = limit
            }
        , tracksResponseTracks = tracks
        }

    tracksStatsHandler :: AppM TracksStats
    tracksStatsHandler = do
      (total, matched, locked) <- liftIO $ withConnection pool getLibraryTrackStats
      pure $ TracksStats
        { tracksStatsTotal = total
        , tracksStatsMatched = matched
        , tracksStatsUnmatched = total - matched
        , tracksStatsLocked = locked
        }

    updateTrackHandler :: Int64 -> UpdateTrackRequest -> AppM NoContent
    updateTrackHandler trackId req = do
      liftIO $ withConnection pool $ \conn ->
        setTrackClusterAssignment conn trackId (updateTrackClusterId req)
      pure NoContent
