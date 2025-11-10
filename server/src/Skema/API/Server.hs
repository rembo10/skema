{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | API server implementation.
module Skema.API.Server
  ( app
  , startServer
  ) where

import Skema.API.Types
import Skema.API.Handlers.Auth (authServer)
import Skema.API.Handlers.Library (libraryServer)
import Skema.API.Handlers.Config (configServer)
import Skema.API.Handlers.Clusters (clustersServer)
import Skema.API.Handlers.Stats (statsServer)
import Skema.API.Handlers.Acquisition (acquisitionServer)
import Skema.API.Handlers.Catalog (catalogServer)
import Skema.API.Handlers.Downloads (downloadsServer)
import Skema.API.Handlers.Diffs (diffsServer)
import Skema.API.Handlers.Events (eventsServer)
import Skema.API.Handlers.Filesystem (filesystemServer)
import Skema.API.Handlers.QualityProfiles (qualityProfilesServer)
import Skema.API.Handlers.Static (staticFileServer, frontendServer)
import Skema.Auth (AuthStore, newAuthStore, checkAuthEnabled)
import Skema.Auth.JWT (JWTSecret, getJWTSecret)
import Skema.Database.Connection (ConnectionPool)
import Skema.Config.Types (Config, ServerConfig)
import qualified Skema.Config.Validation as CfgVal
import qualified Skema.Config.Types as Cfg
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Servant
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai (Middleware, requestMethod, rawPathInfo, rawQueryString, responseStatus)
import Network.HTTP.Types (statusCode)
import Katip
import Control.Monad.Catch (catch)
import qualified Control.Exception as E
import Data.List (isInfixOf)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, AsyncCancelled)

-- | Constants
heartbeatIntervalSeconds :: Int
heartbeatIntervalSeconds = 30

-- | Request logging middleware.
--
-- Logs all API requests with method, path, query string, and response time.
requestLoggingMiddleware :: LogEnv -> Middleware
requestLoggingMiddleware le application req responseHandler = do
  startTime <- getCurrentTime

  -- Extract request info
  let method = decodeUtf8 $ requestMethod req
  let path = decodeUtf8 $ rawPathInfo req
  let query = decodeUtf8 $ rawQueryString req

  -- Call the app and capture the response
  application req $ \response -> do
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    let durationMs = round (duration * 1000) :: Int
    let status = statusCode (responseStatus response)

    -- Color code status: green for 2xx, red for everything else
    let coloredStatus = if status >= 200 && status < 300
          then "\ESC[32m" <> show status <> "\ESC[0m"  -- Green for success
          else "\ESC[31m" <> show status <> "\ESC[0m"  -- Red for errors

    -- Log the request with colored status code
    let initialContext = ()
    let initialNamespace = "api"
    runKatipContextT le initialContext initialNamespace $ do
      $(logTM) InfoS $ logStr $
        method <> " " <> path <> query <> " - " <> coloredStatus <> " - " <> (show durationMs :: Text) <> "ms"

    responseHandler response

-- | WAI application with logging middleware.
app :: LogEnv -> EventBus -> AuthStore -> ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> Maybe Text -> FilePath -> FilePath -> Application
app le bus authStore serverCfg jwtSecret registry connPool libPath cacheDir configPath =
  requestLoggingMiddleware le $
    serve (Proxy :: Proxy API) (server le bus authStore serverCfg jwtSecret registry connPool libPath cacheDir (srConfigVar registry) configPath)

-- | Servant server.
server :: LogEnv -> EventBus -> AuthStore -> ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> Maybe Text -> FilePath -> TVar Config -> FilePath -> Server API
server le bus authStore serverCfg jwtSecret registry connPool libPath cacheDir configVar configPath =
  (authServer authStore jwtSecret configVar
   :<|> libraryServer le bus serverCfg jwtSecret registry connPool configVar
   :<|> configServer le bus serverCfg jwtSecret connPool configVar configPath
   :<|> diffsServer le bus serverCfg jwtSecret registry connPool configVar
   :<|> clustersServer le serverCfg jwtSecret registry connPool configVar
   :<|> statsServer serverCfg jwtSecret connPool configVar
   :<|> acquisitionServer serverCfg jwtSecret connPool configVar
   :<|> catalogServer le bus serverCfg jwtSecret registry connPool cacheDir configVar
   :<|> downloadsServer le bus serverCfg jwtSecret connPool (srDownloadProgressMap registry) configVar
   :<|> eventsServer le bus serverCfg jwtSecret connPool libPath configVar
   :<|> filesystemServer serverCfg jwtSecret configVar
   :<|> qualityProfilesServer serverCfg jwtSecret connPool configVar)
  :<|> staticFileServer cacheDir
  :<|> frontendServer

-- | Start a background worker that emits heartbeat events periodically.
-- This keeps SSE connections alive during quiet periods.
startHeartbeatWorker :: LogEnv -> EventBus -> IO ()
startHeartbeatWorker le bus = do
  _ <- async $ forever $ do
    -- Wait between heartbeats
    threadDelay (heartbeatIntervalSeconds * 1000000)

    -- Emit heartbeat event
    EventBus.publishAndLog bus le "heartbeat" Events.Heartbeat
  pure ()

-- | Start the HTTP server with logging and exception handling.
startServer :: LogEnv -> EventBus -> ServerConfig -> ServiceRegistry -> ConnectionPool -> Maybe Text -> FilePath -> FilePath -> Maybe Int -> IO ()
startServer le bus serverCfg registry connPool libPath cacheDir configPath cliPort = do
  let initialContext = ()
  let initialNamespace = "server"
  let host = Cfg.serverHost serverCfg
  port <- CfgVal.getServerPort cliPort serverCfg

  runKatipContextT le initialContext initialNamespace $ do
    -- Load JWT secret from config
    maybeJwtSecret <- liftIO $ getJWTSecret serverCfg
    jwtSecret <- case maybeJwtSecret of
      Nothing -> do
        $(logTM) ErrorS "Failed to load JWT secret from config"
        liftIO exitFailure
      Just secret -> pure secret

    $(logTM) InfoS "JWT secret loaded successfully"

    -- Create auth store
    authStore <- liftIO newAuthStore

    -- Start heartbeat worker for SSE keep-alive
    liftIO $ startHeartbeatWorker le bus
    $(logTM) InfoS $ logStr ("Heartbeat worker started" :: Text)

    -- Log server startup
    $(logTM) InfoS $ logStr $ "Starting Skema API server on " <> host <> ":" <> show port

    -- Check if auth is enabled
    authEnabled <- liftIO $ checkAuthEnabled serverCfg
    if authEnabled
      then $(logTM) InfoS "Authentication enabled"
      else $(logTM) WarningS "Authentication not configured - API will be open"

    -- Configure Warp settings
    let settings = setHost (fromString $ toString host)
                 $ setPort port
                 $ defaultSettings

    -- Run server with exception handling
    liftIO (runSettings settings (app le bus authStore serverCfg jwtSecret registry connPool libPath cacheDir configPath)) `catch` handleStartupException host port
  where
    handleStartupException :: Text -> Int -> E.SomeException -> KatipContextT IO ()
    handleStartupException _h p ex =
      -- AsyncCancelled is expected during shutdown, treat as normal
      case E.fromException ex of
        Just (_ :: AsyncCancelled) ->
          $(logTM) InfoS $ logStr ("Server shutdown requested" :: Text)
        Nothing
          | "resource busy" `isInfixOf` exMsg || "Address already in use" `isInfixOf` exMsg -> do
              $(logTM) ErrorS $ logStr ("Failed to start server: Port " <> show p <> " is already in use" :: Text)
              liftIO exitFailure
          | otherwise -> do
              $(logTM) ErrorS $ logStr ("Failed to start server: " <> toText exMsg :: Text)
              liftIO exitFailure
      where
        exMsg = show ex
