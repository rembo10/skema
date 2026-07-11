{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | API server implementation.
module Skema.API.Server
  ( app
  , startServer
  ) where

import Skema.API.Types
import Skema.API.OpenApi (openApiSpec, swaggerUiHtml)
import Skema.API.Handlers.Auth (authServer)
import Skema.API.Handlers.Library (libraryServer)
import Skema.API.Handlers.Config (configServer, configSchemaServer)
import Skema.API.Handlers.Clusters (clustersServer)
import Skema.API.Handlers.Stats (statsServer)
import Skema.API.Handlers.Acquisition (acquisitionServer)
import Skema.API.Handlers.Catalog (catalogServer)
import Skema.API.Handlers.Downloads (downloadsServer)
import Skema.API.Handlers.Diffs (diffsServer)
import Skema.API.Handlers.Events (eventsServer)
import Skema.API.Handlers.Filesystem (filesystemServer)
import Skema.API.Handlers.QualityProfiles (qualityProfilesServer)
import Skema.API.Handlers.Tasks (tasksServer)
import Skema.API.Handlers.Version (versionServer, LatestRelease)
import Skema.API.Handlers.Static (staticFileServer, frontendServer)
import Skema.Services.TaskManager (TaskManager)
import qualified Skema.Services.TaskManager as TM
import Skema.Auth (AuthStore, newAuthStore, checkAuthEnabled)
import Skema.Auth.JWT (JWTSecret, getJWTSecret, validateJWT)
import Skema.Database.Connection (ConnectionPool)
import Skema.Config.Types (Config, ServerConfig)
import qualified Skema.Config.Types as Cfg
import qualified Skema.Config.Validation as CfgVal
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Servant
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setHost, setPort)
import Network.Wai (Middleware, Request, requestMethod, rawPathInfo, rawQueryString, responseStatus, requestHeaders, responseLBS)
import Network.HTTP.Types (statusCode, status401)
import Network.HTTP.Types.Header (hAuthorization)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Katip
import Control.Monad.Catch (catch)
import qualified Control.Exception as E
import Data.List (isInfixOf, lookup)
import Data.Time (getCurrentTime, diffUTCTime)
import Skema.API.Handlers.Version (startUpdateChecker)
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

-- | Routes reachable without authentication.
--
-- These are either genuinely public (login, version, docs, config schema) or
-- authenticated by a @?token=@ query parameter inside the handler, because
-- @EventSource@ cannot send custom headers (the SSE streams).
isPublicRequest :: Request -> Bool
isPublicRequest req =
  let path = rawPathInfo req
      method = requestMethod req
  in not ("/api/" `BS.isPrefixOf` path)                              -- static assets, images, SPA
     || path `elem` publicApiPaths
     || (method == "GET" && path == "/api/events")                   -- SSE (token-authed in handler)
     || (method == "GET" && "/releases/stream" `BS.isSuffixOf` path) -- SSE (token-authed in handler)
  where
    publicApiPaths =
      [ "/api/auth/status"
      , "/api/auth/credentials"
      , "/api/version"
      , "/api/config/schema"
      , "/api/docs"
      , "/api/openapi.json"
      ]

-- | Central authentication middleware (default-deny for @/api@).
--
-- Authentication stays disabled until the user configures credentials, matching
-- the first-run setup flow: while no credentials are set, every request passes
-- through. Once credentials exist, any @/api@ request outside 'isPublicRequest'
-- must present either a valid bearer JWT or the configured @X-API-Key@.
--
-- Enforcing auth here rather than per-handler means a route is closed by default
-- — forgetting to wire auth into a new endpoint can no longer expose it.
authMiddleware :: TVar Config -> JWTSecret -> Middleware
authMiddleware configVar jwtSecret application req responseHandler = do
  cfg <- Cfg.server <$> readTVarIO configVar
  authEnabled <- checkAuthEnabled cfg
  authorized <- if not authEnabled || isPublicRequest req
    then pure True
    else isAuthorized cfg
  if authorized
    then application req responseHandler
    else responseHandler $ responseLBS status401 [] "Authorization header required"
  where
    isAuthorized cfg
      | apiKeyMatches cfg = pure True
      | otherwise = case bearerToken of
          Nothing -> pure False
          Just token -> either (const False) (const True) <$> validateJWT jwtSecret token

    apiKeyMatches cfg = case (Cfg.serverApiKey cfg, lookup "X-API-Key" (requestHeaders req)) of
      (Just expected, Just provided) -> TE.encodeUtf8 expected == provided
      _ -> False

    bearerToken =
      decodeUtf8 <$> (lookup hAuthorization (requestHeaders req) >>= BS.stripPrefix "Bearer ")

-- | WAI application with logging and central authentication middleware.
app :: LogEnv -> EventBus -> AuthStore -> ServerConfig -> JWTSecret -> ServiceRegistry -> TaskManager -> ConnectionPool -> Maybe Text -> FilePath -> FilePath -> TVar (Maybe LatestRelease) -> Application
app le bus authStore serverCfg jwtSecret registry tm connPool libPath cacheDir configPath latestVar =
  requestLoggingMiddleware le $
  authMiddleware (srConfigVar registry) jwtSecret $
    serve (Proxy :: Proxy API) (server le bus authStore serverCfg jwtSecret registry tm connPool libPath cacheDir (srConfigVar registry) configPath latestVar)

-- | Servant server.
server :: LogEnv -> EventBus -> AuthStore -> ServerConfig -> JWTSecret -> ServiceRegistry -> TaskManager -> ConnectionPool -> Maybe Text -> FilePath -> TVar Config -> FilePath -> TVar (Maybe LatestRelease) -> Server API
server le bus authStore serverCfg jwtSecret registry tm connPool libPath cacheDir configVar configPath latestVar =
  ((authServer authStore jwtSecret configVar (srClock registry)
   :<|> libraryServer le bus serverCfg registry tm connPool configVar
   :<|> configServer le bus serverCfg connPool configVar configPath
   :<|> configSchemaServer
   :<|> diffsServer le bus serverCfg registry connPool
   :<|> clustersServer le bus serverCfg registry tm connPool configVar
   :<|> statsServer serverCfg connPool configVar
   :<|> acquisitionServer le bus serverCfg connPool (srMBClientEnv registry) (srHttpClient registry) tm
   :<|> catalogServer le bus serverCfg jwtSecret registry tm connPool cacheDir configVar
   :<|> downloadsServer le bus serverCfg registry tm connPool (srDownloadProgressMap registry) configVar
   :<|> eventsServer le bus serverCfg jwtSecret connPool libPath configVar
   :<|> filesystemServer serverCfg
   :<|> qualityProfilesServer serverCfg connPool
   :<|> tasksServer tm)
   :<|> versionServer latestVar
   :<|> (pure swaggerUiHtml :<|> pure openApiSpec))
  :<|> staticFileServer bus le cacheDir
  :<|> frontendServer configVar

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
  host <- CfgVal.getServerHost serverCfg
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

    -- Create task manager
    taskManager <- liftIO $ TM.newTaskManager bus le (srClock registry)
    $(logTM) InfoS $ logStr ("Task manager initialized" :: Text)

    -- Start heartbeat worker for SSE keep-alive
    liftIO $ startHeartbeatWorker le bus
    $(logTM) InfoS $ logStr ("Heartbeat worker started" :: Text)

    -- Start update checker
    latestVar <- liftIO $ newTVarIO Nothing
    liftIO $ startUpdateChecker le (srHttpClient registry) (srConfigVar registry) latestVar
    $(logTM) InfoS $ logStr ("Update checker started" :: Text)

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
    liftIO (runSettings settings (app le bus authStore serverCfg jwtSecret registry taskManager connPool libPath cacheDir configPath latestVar)) `catch` handleStartupException host port
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
