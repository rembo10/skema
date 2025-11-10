{-# LANGUAGE OverloadedStrings #-}

-- | Library API handlers.
module Skema.API.Handlers.Library
  ( libraryServer
  ) where

import Skema.API.Types.Library (LibraryAPI, ScanResponse(..))
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
libraryServer le bus _serverCfg jwtSecret _registry _pool configVar = \maybeAuthHeader ->
  scanHandler maybeAuthHeader
  :<|> filesHandler maybeAuthHeader
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
