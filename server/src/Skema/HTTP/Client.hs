{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

-- | Global HTTP client with per-domain rate limiting, retries, and error handling.
module Skema.HTTP.Client
  ( -- * Types
    HttpClient
  , HttpConfig(..)
  , HttpError(..)
  , HttpAuth(..)
  , DomainConfig(..)
  , RetryConfig(..)
  , UserAgentFormat(..)
  , UserAgentData(..)
    -- * Client creation
  , newHttpClient
  , defaultHttpConfig
  , defaultRetryConfig
  , defaultDomainConfig
  , defaultUserAgentData
  , musicBrainzDomainConfig
  , headphonesVIPDomainConfig
    -- * User agent formatting
  , formatUserAgent
    -- * Client accessors
  , getManager
    -- * Making requests
  , makeRequest
  , makeRequestWithRetry
    -- * Convenience methods
  , get
  , getWithBasicAuth
  , post
  , getJSON
  , getJSONWithBasicAuth
  , postJSON
  , prettyHttpError
  ) where

import Relude hiding (get)
import qualified Data.List as List

import Network.HTTP.Client (Manager, Request, Response, httpLbs, responseBody, responseStatus, responseHeaders, parseRequest, requestHeaders, method, requestBody, RequestBody(..), newManager, applyBasicAuth, host, path, queryString, secure)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hUserAgent, hRetryAfter, hAuthorization)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent.STM ()
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent as Concurrent
import Control.Exception (try)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Katip
import qualified Paths_skema as Paths
import Data.Version (showVersion)

-- | User agent format string with placeholders
-- Supported placeholders: {appName}, {version}, {url}, {description}
-- Example: "{appName}/{version} ( {url} )" -> "Skema/0.1.0 ( https://github.com/rembo10/skema )"
newtype UserAgentFormat = UserAgentFormat Text
  deriving (Show, Eq)

-- | Data to fill user agent format placeholders
data UserAgentData = UserAgentData
  { uaAppName :: Text        -- ^ Application name (e.g., "Skema")
  , uaVersion :: Text        -- ^ Version (e.g., "0.1.0")
  , uaUrl :: Text           -- ^ Project URL
  , uaDescription :: Text   -- ^ Description
  } deriving (Show, Eq)

-- | HTTP authentication types
data HttpAuth
  = BasicAuth Text Text       -- ^ HTTP Basic authentication (username, password)
  | BearerToken Text          -- ^ Bearer token authentication
  | ApiKeyHeader Text Text    -- ^ API key in custom header (header name, key value)
  deriving (Show, Eq)

-- | Per-domain configuration
data DomainConfig = DomainConfig
  { domainUserAgentFormat :: Maybe UserAgentFormat
    -- ^ Custom user agent format for this domain (Nothing = use default)
  , domainRateLimit :: Double
    -- ^ Rate limit in requests per second
  , domainAuth :: Maybe HttpAuth
    -- ^ Authentication for this domain
  } deriving (Show, Eq)

-- | HTTP error types
data HttpError
  = HttpStatusError Int Text (Maybe Int)  -- ^ Non-2xx status code with body and optional Retry-After (microseconds)
  | HttpParseError Text       -- ^ Failed to parse URL
  | HttpNetworkError Text     -- ^ Network/connection error
  | HttpTimeoutError          -- ^ Request timed out
  | HttpDecodeError Text      -- ^ Failed to decode JSON response
  | HttpRateLimitExceeded Text (Maybe Int) -- ^ Rate limited by server with optional Retry-After (microseconds)
  deriving (Show, Eq)

-- | Pretty print HTTP error for user display
prettyHttpError :: HttpError -> Text
prettyHttpError (HttpStatusError code body _) =
  "HTTP " <> show code <> " error" <> if T.null body then "" else ": " <> T.take 200 body
prettyHttpError (HttpParseError msg) = "Invalid URL: " <> msg
prettyHttpError (HttpNetworkError msg) = "Network error: " <> msg
prettyHttpError HttpTimeoutError = "Request timed out"
prettyHttpError (HttpDecodeError msg) = "Failed to decode response: " <> msg
prettyHttpError (HttpRateLimitExceeded msg _) = "Rate limited: " <> msg

-- | Configuration for HTTP client
data HttpConfig = HttpConfig
  { httpDefaultUserAgentFormat :: UserAgentFormat
    -- ^ Default user agent format for requests
  , httpDomainConfigs :: Map Text DomainConfig
    -- ^ Per-domain configurations (user agent, rate limits, auth)
  , httpDefaultRateLimit :: Double
    -- ^ Default rate limit in requests per second (fallback for unconfigured domains)
  , httpMaxRetries :: Int
    -- ^ Maximum number of retries for failed requests
  , httpRetryConfig :: RetryConfig
    -- ^ Retry configuration
  } deriving (Show, Eq)

-- | Retry configuration
data RetryConfig = RetryConfig
  { retryInitialDelay :: Int
    -- ^ Initial retry delay in microseconds (e.g., 100000 = 0.1s)
  , retryMaxDelay :: Int
    -- ^ Maximum retry delay in microseconds (e.g., 10000000 = 10s)
  , retryMultiplier :: Double
    -- ^ Multiplier for exponential backoff (e.g., 2.0 doubles each time)
  , retryableStatuses :: [Int]
    -- ^ HTTP status codes that should trigger a retry (e.g., [429, 500, 502, 503, 504])
  } deriving (Show, Eq)

-- | Format a user agent string by replacing placeholders
-- Supported placeholders: {appName}, {version}, {url}, {description}
formatUserAgent :: UserAgentFormat -> UserAgentData -> Text
formatUserAgent (UserAgentFormat format) UserAgentData{..} =
  format
    & T.replace "{appName}" uaAppName
    & T.replace "{version}" uaVersion
    & T.replace "{url}" uaUrl
    & T.replace "{description}" uaDescription

-- | Default user agent data (reads version from cabal)
defaultUserAgentData :: UserAgentData
defaultUserAgentData = UserAgentData
  { uaAppName = "Skema"
  , uaVersion = toText $ showVersion Paths.version
  , uaUrl = "https://github.com/rembo10/skema"
  , uaDescription = "Music Library Manager"
  }

-- | Default domain configuration
defaultDomainConfig :: DomainConfig
defaultDomainConfig = DomainConfig
  { domainUserAgentFormat = Nothing
  , domainRateLimit = 2.0
  , domainAuth = Nothing
  }

-- | MusicBrainz-compliant domain configuration (official server)
-- Format: "AppName/Version ( url )" as required by MusicBrainz API
musicBrainzDomainConfig :: DomainConfig
musicBrainzDomainConfig = DomainConfig
  { domainUserAgentFormat = Just $ UserAgentFormat "{appName}/{version} ( {url} )"
  , domainRateLimit = 1.0  -- MusicBrainz requires 1 req/sec
  , domainAuth = Nothing
  }

-- | Headphones VIP MusicBrainz mirror configuration
-- Same user agent format as official, but 3 req/sec with basic auth
headphonesVIPDomainConfig :: Text -> Text -> DomainConfig
headphonesVIPDomainConfig username password = DomainConfig
  { domainUserAgentFormat = Just $ UserAgentFormat "{appName}/{version} ( {url} )"
  , domainRateLimit = 3.0  -- VIP mirror allows 3 req/sec
  , domainAuth = Just $ BasicAuth username password
  }

-- | Default HTTP configuration
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpDefaultUserAgentFormat = UserAgentFormat "{appName}/{version} ({description})"
  , httpDomainConfigs = Map.fromList
      [ ("musicbrainz.org", musicBrainzDomainConfig)
      , ("api.musicbrainz.org", musicBrainzDomainConfig)
      ]
  , httpDefaultRateLimit = 2.0  -- 2 requests per second default
  , httpMaxRetries = 3
  , httpRetryConfig = defaultRetryConfig
  }

-- | Default retry configuration
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryInitialDelay = 100000      -- 0.1 seconds
  , retryMaxDelay = 10000000         -- 10 seconds
  , retryMultiplier = 2.0            -- Double each time
  , retryableStatuses = [429, 500, 502, 503, 504]
  }

-- | HTTP client with rate limiting and retry logic
data HttpClient = HttpClient
  { hcManager :: Manager
  , hcConfig :: HttpConfig
  , hcUserAgentData :: UserAgentData
    -- ^ Data for formatting user agent strings
  , hcDomainLocks :: TVar (Map Text (MVar UTCTime))
    -- ^ Per-domain locks for rate limiting. Each domain has a lock that holds the last request time.
    -- ^ Requests queue up waiting for the lock, ensuring serialized access with proper delays.
  , hcLogEnv :: LogEnv
    -- ^ Logging environment
  }

-- | Create a new HTTP client with rate limiting
newHttpClient :: LogEnv -> HttpConfig -> UserAgentData -> IO HttpClient
newHttpClient logEnv config uaData = do
  manager <- newManager tlsManagerSettings
  locks <- STM.newTVarIO Map.empty
  pure $ HttpClient
    { hcManager = manager
    , hcConfig = config
    , hcUserAgentData = uaData
    , hcDomainLocks = locks
    , hcLogEnv = logEnv
    }

-- | Get the HTTP manager from a client
getManager :: HttpClient -> Manager
getManager = hcManager

-- | Extract domain from a URL
extractDomain :: Text -> Maybe Text
extractDomain url =
  case T.breakOn "//" url of
    (_, rest) ->
      let afterProtocol = T.drop 2 rest
          domain = T.takeWhile (\c -> c /= '/' && c /= ':') afterProtocol
      in if T.null domain then Nothing else Just domain

-- | Normalize domain for rate limiting purposes
-- Multiple subdomains of the same service should share a rate limit lock
normalizeDomainForRateLimiting :: Text -> Text
normalizeDomainForRateLimiting domain
  -- MusicBrainz: all subdomains share the same rate limit
  | "musicbrainz.org" `T.isSuffixOf` domain = "musicbrainz.org"
  | otherwise = domain

-- | Get domain configuration (or default)
getDomainConfig :: HttpConfig -> Text -> DomainConfig
getDomainConfig config domain =
  fromMaybe defaultDomainConfig $ Map.lookup domain (httpDomainConfigs config)

-- | Get the rate limit for a domain (in seconds between requests)
getRateLimitDelay :: HttpConfig -> Text -> Double
getRateLimitDelay config domain =
  let domainCfg = getDomainConfig config domain
      rps = domainRateLimit domainCfg
  in 1.0 / rps  -- Convert requests per second to seconds per request

-- | Get the user agent string for a domain
getUserAgentForDomain :: HttpClient -> Text -> Text
getUserAgentForDomain client domain =
  let config = hcConfig client
      uaData = hcUserAgentData client
      domainCfg = getDomainConfig config domain
      uaFormat = fromMaybe (httpDefaultUserAgentFormat config) (domainUserAgentFormat domainCfg)
  in formatUserAgent uaFormat uaData

-- | Apply authentication to a request for a domain
applyAuthForDomain :: HttpClient -> Text -> Request -> Request
applyAuthForDomain client domain req =
  let config = hcConfig client
      domainCfg = getDomainConfig config domain
  in case domainAuth domainCfg of
       Nothing -> req
       Just (BasicAuth username password) ->
         applyBasicAuth (TE.encodeUtf8 username) (TE.encodeUtf8 password) req
       Just (BearerToken token) ->
         req { requestHeaders = (hAuthorization, "Bearer " <> TE.encodeUtf8 token) : requestHeaders req }
       Just (ApiKeyHeader headerName key) ->
         req { requestHeaders = (fromString $ toString headerName, TE.encodeUtf8 key) : requestHeaders req }

-- | Get or create a lock for a domain
-- Locks are created lazily on first use and stored in the client
getDomainLock :: HttpClient -> Text -> IO (MVar UTCTime)
getDomainLock client domain = do
  locks <- STM.readTVarIO (hcDomainLocks client)
  case Map.lookup domain locks of
    Just lock -> pure lock
    Nothing -> do
      -- Create a new lock for this domain
      -- Initialize with a time far in the past so first request proceeds immediately
      now <- getCurrentTime
      let longAgo = addUTCTime (-1000) now  -- 1000 seconds ago
      newLock <- Concurrent.newMVar longAgo

      -- Store it atomically (may race with other threads, but that's ok - one will win)
      STM.atomically $ do
        locks' <- STM.readTVar (hcDomainLocks client)
        case Map.lookup domain locks' of
          Just existingLock -> pure existingLock  -- Another thread created it first
          Nothing -> do
            STM.writeTVar (hcDomainLocks client) (Map.insert domain newLock locks')
            pure newLock

-- | Acquire rate limit lock, wait if necessary, then release it
-- This enforces rate limiting on request START times while allowing concurrent execution
acquireAndReleaseRateLimitLock :: HttpClient -> Text -> IO ()
acquireAndReleaseRateLimitLock client domain = do
  let delay = getRateLimitDelay (hcConfig client) domain
      -- Normalize domain for rate limiting so subdomains share the same lock
      normalizedDomain = normalizeDomainForRateLimiting domain
  lock <- getDomainLock client normalizedDomain

  -- Take the lock (blocks if another request is in queue for this domain)
  -- This is where concurrent requests queue up!
  lastRequestTime <- Concurrent.takeMVar lock

  -- Check how long it's been since the last request
  now <- getCurrentTime
  let timeSinceLastRequest = realToFrac $ diffUTCTime now lastRequestTime :: Double

  -- Wait if we need to respect the rate limit
  when (timeSinceLastRequest < delay) $ do
    let waitTime = ceiling ((delay - timeSinceLastRequest) * 1000000) :: Int
    runKatipContextT (hcLogEnv client) () "http-client" $ do
      $(logTM) DebugS $ logStr $ "Rate limiting: waiting " <> show (waitTime `div` 1000) <> "ms for " <> normalizedDomain
    threadDelay waitTime

  -- Update the lock with current time and release it immediately
  -- This allows the next request to start its delay calculation while we're still downloading
  now' <- getCurrentTime
  Concurrent.putMVar lock now'

-- | Parse Retry-After header value (either seconds or HTTP date)
parseRetryAfter :: LBS.ByteString -> Maybe Int
parseRetryAfter bs =
  case readMaybe (toString $ TE.decodeUtf8 $ LBS.toStrict bs) of
    Just seconds -> Just (seconds * 1000000)  -- Convert to microseconds
    Nothing -> Nothing  -- TODO: Parse HTTP date format

-- | Make an HTTP request with rate limiting (no retry)
makeRequest :: HttpClient -> Request -> IO (Either HttpError (Response LBS.ByteString))
makeRequest client req = do
  let domain = TE.decodeUtf8 (host req)
      protocol = if secure req then "https://" else "http://"
      url = protocol <> domain <> TE.decodeUtf8 (path req) <> TE.decodeUtf8 (queryString req)

  -- Log request
  runKatipContextT (hcLogEnv client) () "http-client" $ do
    $(logTM) DebugS $ logStr $ ("HTTP " <> TE.decodeUtf8 (method req) <> " " <> url :: Text)

  -- Enforce rate limiting on request start time, then release lock
  -- This allows concurrent requests while still respecting rate limits
  acquireAndReleaseRateLimitLock client domain

  -- Make the request (lock is now released, allowing concurrent requests to execute)
  try (httpLbs req (hcManager client)) >>= \case
    Left (err :: SomeException) -> do
      let errText = show err :: String
      runKatipContextT (hcLogEnv client) () "http-client" $ do
        $(logTM) ErrorS $ logStr $ ("HTTP request failed: " <> toText errText :: Text)
      pure $ Left $ HttpNetworkError (toText errText)

    Right response -> do
      let status = responseStatus response
          statusNum = statusCode status
          body = responseBody response
          headers = responseHeaders response
          -- Extract Retry-After header if present
          retryAfter = do
            retryHeaderValue <- List.lookup hRetryAfter headers
            parseRetryAfter (LBS.fromStrict retryHeaderValue)

      runKatipContextT (hcLogEnv client) () "http-client" $ do
        $(logTM) DebugS $ logStr $ ("HTTP " <> show statusNum <> " (" <> show (LBS.length body) <> " bytes)" :: Text)
        when (isJust retryAfter) $ do
          $(logTM) DebugS $ logStr $ ("Retry-After header: " <> show (fmap (\d -> d `div` 1000) retryAfter) <> "ms" :: Text)

      if statusNum >= 200 && statusNum < 300
        then pure $ Right response
        else do
          let bodyText = TE.decodeUtf8 (LBS.toStrict body)
          if statusNum == 429
            then pure $ Left $ HttpRateLimitExceeded bodyText retryAfter
            else pure $ Left $ HttpStatusError statusNum bodyText retryAfter

-- | Make an HTTP request with automatic retries on failure
makeRequestWithRetry :: HttpClient -> Request -> IO (Either HttpError (Response LBS.ByteString))
makeRequestWithRetry client req = go 0 initialDelay
  where
    maxRetries = httpMaxRetries (hcConfig client)
    retryConfig = httpRetryConfig (hcConfig client)
    retryableStatuses' = retryableStatuses retryConfig
    domain = TE.decodeUtf8 (host req)
    pathStr = TE.decodeUtf8 (path req)
    queryStr = TE.decodeUtf8 (queryString req)
    -- Format URL nicely for logs: domain + path (truncated if too long) + query indicator
    formattedUrl = domain <> (if T.length pathStr > 50 then T.take 47 pathStr <> "..." else pathStr) <>
                   (if T.null queryStr then "" else "?...")
    -- Use normalized domain for rate limit calculation to account for shared locks
    normalizedDomain = normalizeDomainForRateLimiting domain
    domainRateLimitMicros = ceiling (getRateLimitDelay (hcConfig client) normalizedDomain * 1000000) :: Int
    -- Initial delay should be at least the domain's rate limit to avoid redundant delays
    initialDelay = max (retryInitialDelay retryConfig) domainRateLimitMicros

    go :: Int -> Int -> IO (Either HttpError (Response LBS.ByteString))
    go attempt currentDelay = do
      result <- makeRequest client req
      case result of
        Right response -> pure $ Right response
        Left err@(HttpStatusError code _body retryAfter)
          | attempt < maxRetries && code `elem` retryableStatuses' -> do
              -- Use Retry-After header if present, otherwise let rate limiter handle it
              case retryAfter of
                Just delayMicros -> do
                  runKatipContextT (hcLogEnv client) () "http-client" $ do
                    $(logTM) WarningS $ logStr $
                      ("Request to " <> formattedUrl <> " failed with " <> show code <>
                      ", retrying in " <> show (delayMicros `div` 1000) <>
                      "ms (from Retry-After header, attempt " <>
                      show (attempt + 1) <> "/" <> show maxRetries <> ")" :: Text)
                  threadDelay delayMicros
                  let nextDelay = min (retryMaxDelay retryConfig)
                                      (floor $ fromIntegral currentDelay * retryMultiplier retryConfig)
                  go (attempt + 1) nextDelay
                Nothing -> do
                  -- No Retry-After header - let rate limiter handle the delay
                  runKatipContextT (hcLogEnv client) () "http-client" $ do
                    $(logTM) WarningS $ logStr $
                      ("Request to " <> formattedUrl <> " failed with " <> show code <>
                      ", will retry (attempt " <> show (attempt + 1) <> "/" <> show maxRetries <>
                      "), rate limiter will enforce delay" :: Text)
                  -- No threadDelay here - the rate limiter will ensure proper spacing
                  let nextDelay = min (retryMaxDelay retryConfig)
                                      (floor $ fromIntegral currentDelay * retryMultiplier retryConfig)
                  go (attempt + 1) nextDelay
          | otherwise -> pure $ Left err

        Left err@(HttpRateLimitExceeded _ retryAfter)
          | attempt < maxRetries -> do
              -- For 429 errors, use Retry-After if provided, otherwise use exponential backoff
              let rateLimitDelay = fromMaybe currentDelay retryAfter
              runKatipContextT (hcLogEnv client) () "http-client" $ do
                $(logTM) WarningS $ logStr $
                  ("Request to " <> formattedUrl <> " rate limited (429), waiting " <>
                  show (rateLimitDelay `div` 1000) <> "ms" <>
                  (if isJust retryAfter then " (from Retry-After)" else "") <>
                  " (attempt " <> show (attempt + 1) <> "/" <> show maxRetries <> ")" :: Text)
              threadDelay rateLimitDelay
              let nextDelay = min (retryMaxDelay retryConfig)
                                  (floor $ fromIntegral currentDelay * retryMultiplier retryConfig)
              go (attempt + 1) nextDelay
          | otherwise -> pure $ Left err

        Left err@(HttpNetworkError _)
          | attempt < maxRetries -> do
              runKatipContextT (hcLogEnv client) () "http-client" $ do
                $(logTM) WarningS $ logStr $
                  ("Request to " <> formattedUrl <> " network error, retrying in " <>
                  show (currentDelay `div` 1000) <> "ms" :: Text)
              threadDelay currentDelay
              let nextDelay = min (retryMaxDelay retryConfig)
                                  (floor $ fromIntegral currentDelay * retryMultiplier retryConfig)
              go (attempt + 1) nextDelay
          | otherwise -> pure $ Left err

        Left err -> pure $ Left err

-- | Convenience method: GET request
get :: HttpClient -> Text -> IO (Either HttpError LBS.ByteString)
get client url = do
  case parseRequest (toString url) of
    Nothing -> pure $ Left $ HttpParseError ("Invalid URL: " <> url)
    Just req -> do
      let domain = fromMaybe "unknown" $ extractDomain url
          userAgent = getUserAgentForDomain client domain
          reqWithUA = req
            { requestHeaders = (hUserAgent, TE.encodeUtf8 userAgent) : requestHeaders req
            , method = "GET"
            }
          reqWithAuth = applyAuthForDomain client domain reqWithUA
      result <- makeRequestWithRetry client reqWithAuth
      pure $ fmap responseBody result

-- | Convenience method: GET request with Basic Auth (overrides domain-based auth)
getWithBasicAuth :: HttpClient -> Text -> Text -> Text -> IO (Either HttpError LBS.ByteString)
getWithBasicAuth client url username password = do
  case parseRequest (toString url) of
    Nothing -> pure $ Left $ HttpParseError ("Invalid URL: " <> url)
    Just req -> do
      let domain = fromMaybe "unknown" $ extractDomain url
          userAgent = getUserAgentForDomain client domain
          reqWithUA = req
            { requestHeaders = (hUserAgent, TE.encodeUtf8 userAgent) : requestHeaders req
            , method = "GET"
            }
          -- Apply Basic Auth directly (overrides domain config)
          reqWithAuth = applyBasicAuth (TE.encodeUtf8 username) (TE.encodeUtf8 password) reqWithUA
      result <- makeRequestWithRetry client reqWithAuth
      pure $ fmap responseBody result

-- | Convenience method: POST request
post :: HttpClient -> Text -> LBS.ByteString -> [(Text, Text)] -> IO (Either HttpError LBS.ByteString)
post client url body headers = do
  case parseRequest (toString url) of
    Nothing -> pure $ Left $ HttpParseError ("Invalid URL: " <> url)
    Just req -> do
      let domain = fromMaybe "unknown" $ extractDomain url
          userAgent = getUserAgentForDomain client domain
          reqWithHeaders = req
            { requestHeaders =
                (hUserAgent, TE.encodeUtf8 userAgent) :
                fmap (\(k, v) -> (fromString $ toString k, TE.encodeUtf8 v)) headers <>
                requestHeaders req
            , method = "POST"
            , requestBody = RequestBodyLBS body
            }
          reqWithAuth = applyAuthForDomain client domain reqWithHeaders
      result <- makeRequestWithRetry client reqWithAuth
      pure $ fmap responseBody result

-- | Convenience method: GET request expecting JSON response
getJSON :: FromJSON a => HttpClient -> Text -> IO (Either HttpError a)
getJSON client url = do
  result <- get client url
  case result of
    Left err -> pure $ Left err
    Right body ->
      case decode body of
        Nothing -> do
          -- Log the raw response body for debugging
          let bodyText = TE.decodeUtf8 (LBS.toStrict body)
              bodyPreview = T.take 500 bodyText  -- Limit to first 500 chars
          runKatipContextT (hcLogEnv client) () "http-client" $ do
            $(logTM) ErrorS $ logStr $ ("Failed to decode JSON from " <> url <> ". Response body: " <> bodyPreview :: Text)
          pure $ Left $ HttpDecodeError "Failed to parse JSON response"
        Just val -> pure $ Right val

-- | Convenience method: GET request with Basic Auth expecting JSON response
getJSONWithBasicAuth :: FromJSON a => HttpClient -> Text -> Text -> Text -> IO (Either HttpError a)
getJSONWithBasicAuth client url username password = do
  result <- getWithBasicAuth client url username password
  case result of
    Left err -> pure $ Left err
    Right body ->
      case decode body of
        Nothing -> do
          -- Log the raw response body for debugging
          let bodyText = TE.decodeUtf8 (LBS.toStrict body)
              bodyPreview = T.take 500 bodyText  -- Limit to first 500 chars
          runKatipContextT (hcLogEnv client) () "http-client" $ do
            $(logTM) ErrorS $ logStr $ ("Failed to decode JSON from " <> url <> ". Response body: " <> bodyPreview :: Text)
          pure $ Left $ HttpDecodeError "Failed to parse JSON response"
        Just val -> pure $ Right val

-- | Convenience method: POST JSON request expecting JSON response
postJSON :: (ToJSON req, FromJSON resp) => HttpClient -> Text -> req -> IO (Either HttpError resp)
postJSON client url reqData = do
  let body = encode reqData
      headers = [("Content-Type", "application/json")]
  result <- post client url body headers
  case result of
    Left err -> pure $ Left err
    Right respBody ->
      case decode respBody of
        Nothing -> pure $ Left $ HttpDecodeError "Failed to parse JSON response"
        Just val -> pure $ Right val
