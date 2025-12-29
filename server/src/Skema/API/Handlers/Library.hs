{-# LANGUAGE OverloadedStrings #-}

-- | Library API handlers.
module Skema.API.Handlers.Library
  ( libraryServer
  ) where

import Skema.API.Types.Library (LibraryAPI, ScanResponse(..), UpdateTrackRequest(..), TrackWithCluster(..))
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

-- | Library API handlers.
libraryServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> TVar Cfg.Config -> Server LibraryAPI
libraryServer le bus _serverCfg jwtSecret _registry pool configVar = \maybeAuthHeader ->
  scanHandler maybeAuthHeader
  :<|> filesHandler maybeAuthHeader
  :<|> tracksHandler maybeAuthHeader
  :<|> updateTrackHandler maybeAuthHeader
  where
    scanHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      config <- liftIO $ STM.atomically $ STM.readTVar configVar

      liftIO $ do
        -- Get library path
        case Cfg.libraryPath (Cfg.library config) of
          Nothing -> pure $ ScanResponse
            { scanSuccess = False
            , scanMessage = "Library path not configured"
            , scanFilesAdded = 0
            , scanFilesModified = 0
            , scanFilesDeleted = 0
            , scanIdentifyRun = False
            , scanIdentifyTotalGroups = Nothing
            , scanIdentifyMatchedGroups = Nothing
            , scanIdentifyFilesUpdated = Nothing
            }
          Just libOsPath -> do
            libPathText <- OP.decodeUtf libOsPath

            -- Emit library scan requested event
            EventBus.publishAndLog bus le "api" $
              Events.LibraryScanRequested
                { Events.scanPath = toText libPathText
                }

            pure $ ScanResponse
              { scanSuccess = True
              , scanMessage = "Library scan started: " <> toText libPathText
              , scanFilesAdded = 0
              , scanFilesModified = 0
              , scanFilesDeleted = 0
              , scanIdentifyRun = False
              , scanIdentifyTotalGroups = Nothing
              , scanIdentifyMatchedGroups = Nothing
              , scanIdentifyFilesUpdated = Nothing
              }

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
