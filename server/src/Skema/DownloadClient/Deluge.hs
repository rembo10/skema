{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Deluge download client integration.
--
-- Deluge uses a JSON-RPC API over HTTP for its web interface.
-- Authentication is done via a session cookie after login.
module Skema.DownloadClient.Deluge
  ( DelugeClient(..)
  , createDelugeClient
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (void)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), withObject, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.IORef as IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client (RequestBody(..), parseRequest, httpLbs, responseBody, responseStatus, responseCookieJar, CookieJar, method, cookieJar, requestBody, requestHeaders, redirectCount)
import Network.HTTP.Types (statusCode, hContentType)
import Data.List (find)
import Data.Foldable (toList)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS

import Skema.DownloadClient.Types
import Skema.HTTP.Client (HttpClient, getManager, prettyHttpError)
import qualified Skema.HTTP.Client as HTTP

-- | Deluge client configuration
data DelugeClient = DelugeClient
  { delUrl :: Text
  , delPassword :: Text
  , delHttpClient :: HttpClient
  , delCookieJar :: IORef (Maybe CookieJar)
  , delRequestId :: IORef Int
  }

-- | Create a new Deluge client
createDelugeClient :: Text -> Text -> HttpClient -> IO DelugeClient
createDelugeClient url password httpClient = do
  cookieJarRef <- IORef.newIORef Nothing
  requestIdRef <- IORef.newIORef 0
  pure $ DelugeClient url password httpClient cookieJarRef requestIdRef

-- JSON-RPC request/response types
data JsonRpcRequest = JsonRpcRequest
  { rpcMethod :: Text
  , rpcParams :: [Aeson.Value]
  , rpcId :: Int
  } deriving (Show, Generic)

instance ToJSON JsonRpcRequest where
  toJSON JsonRpcRequest{..} = object
    [ "method" .= rpcMethod
    , "params" .= rpcParams
    , "id" .= rpcId
    ]

data JsonRpcResponse a = JsonRpcResponse
  { rpcResult :: Maybe a
  , rpcError :: Maybe Aeson.Value
  , rpcResponseId :: Int
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (JsonRpcResponse a) where
  parseJSON = withObject "JsonRpcResponse" $ \v -> do
    rpcResult <- v .:? "result"
    rpcError <- v .:? "error"
    rpcResponseId <- v .: "id"
    pure JsonRpcResponse{..}

-- Deluge torrent info from web.update_ui
data DelugeTorrent = DelugeTorrent
  { dtHash :: Text
  , dtName :: Text
  , dtState :: Text
  , dtProgress :: Double
  , dtTotalSize :: Maybe Integer
  , dtTotalDone :: Maybe Integer
  , dtRatio :: Maybe Double
  , dtNumSeeds :: Maybe Int
  , dtNumPeers :: Maybe Int
  , dtSavePath :: Maybe Text
  , dtMessage :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON DelugeTorrent where
  parseJSON = withObject "DelugeTorrent" $ \v -> do
    dtHash <- v .: "hash"
    dtName <- v .: "name"
    dtState <- v .: "state"
    dtProgress <- v .: "progress"
    dtTotalSize <- v .:? "total_size"
    dtTotalDone <- v .:? "total_done"
    dtRatio <- v .:? "ratio"
    dtNumSeeds <- v .:? "num_seeds"
    dtNumPeers <- v .:? "num_peers"
    dtSavePath <- v .:? "save_path"
    dtMessage <- v .:? "message"
    pure DelugeTorrent{..}

instance DownloadClientAPI DelugeClient where
  testConnection client = do
    result <- try $ ensureLoggedIn client
    case result of
      Left (err :: SomeException) -> pure $ Left $ T.pack $ show err
      Right () -> pure $ Right ()

  addDownload client AddDownloadRequest{..} = do
    ensureLoggedIn client
    
    -- Determine if this is a magnet link or a URL
    let isMagnet = "magnet:" `T.isPrefixOf` adrUrl
        options = object []  -- Default options
    
    if isMagnet
      then addMagnetLink client adrUrl options
      else do
        -- For URLs (http/https), download the content first to see what we get
        -- Prowlarr may redirect to either a .torrent file or a magnet link
        downloadResult <- HTTP.getFollowRedirects (delHttpClient client) adrUrl
        
        case downloadResult of
          Left err -> 
            pure $ Left $ "Failed to download torrent: " <> prettyHttpError err
          Right responseData -> do
            -- Check if the response is a magnet link (text starting with "magnet:")
            let responseText = TE.decodeUtf8 $ LBS.toStrict responseData
            if "magnet:" `T.isPrefixOf` T.strip responseText
              then do
                -- The response is a magnet link, use it directly
                let magnetLink = T.strip responseText
                addMagnetLink client magnetLink options
              else do
                -- Assume it's a torrent file, encode as base64
                let base64Data = TE.decodeUtf8 $ B64.encode $ LBS.toStrict responseData
                    filename = extractFilename adrUrl
                
                -- Use core.add_torrent_file(filename, filedump_base64, options)
                result <- callRpc client "core.add_torrent_file" 
                  [Aeson.toJSON filename, Aeson.toJSON base64Data, Aeson.toJSON options]
                
                case result of
                  Left err -> pure $ Left $ "Failed to add torrent to Deluge: " <> err
                  Right hashVal -> do
                    let torrentHash = case hashVal of
                          Aeson.String h -> h
                          Aeson.Null -> filename
                          _ -> filename
                    pure $ Right $ AddDownloadResult
                      { adrClientId = torrentHash
                      , adrSuccess = True
                      , adrMessage = Just "Torrent file added to Deluge"
                      }

  getDownloadStatus client downloadId = do
    ensureLoggedIn client
    torrents <- getAllTorrentsInternal client
    case find (\t -> dtHash t == downloadId) torrents of
      Just torrent -> pure $ Right $ delugeTorrentToDownloadInfo torrent
      Nothing -> pure $ Left "Torrent not found"

  getAllDownloads client = do
    ensureLoggedIn client
    torrents <- getAllTorrentsInternal client
    pure $ Right $ map delugeTorrentToDownloadInfo torrents

  pauseDownload client downloadId = do
    ensureLoggedIn client
    result <- callRpc client "core.pause_torrent" [Aeson.toJSON downloadId]
    case result of
      Left err -> pure $ Left err
      Right _ -> pure $ Right ()

  resumeDownload client downloadId = do
    ensureLoggedIn client
    result <- callRpc client "core.resume_torrent" [Aeson.toJSON downloadId]
    case result of
      Left err -> pure $ Left err
      Right _ -> pure $ Right ()

  removeDownload client downloadId deleteFiles = do
    ensureLoggedIn client
    result <- callRpc client "core.remove_torrent" [Aeson.toJSON downloadId, Aeson.toJSON deleteFiles]
    case result of
      Left err -> pure $ Left err
      Right _ -> pure $ Right ()

-- | Helper to add a magnet link to Deluge
addMagnetLink :: DelugeClient -> Text -> Aeson.Value -> IO (Either Text AddDownloadResult)
addMagnetLink client magnetUrl options = do
  result <- callRpc client "core.add_torrent_magnet" [Aeson.toJSON magnetUrl, Aeson.toJSON options]
  case result of
    Left err -> pure $ Left err
    Right hashVal -> do
      let torrentHash = case hashVal of
            Aeson.String h -> h
            _ -> magnetUrl
      pure $ Right $ AddDownloadResult
        { adrClientId = torrentHash
        , adrSuccess = True
        , adrMessage = Just "Magnet link added to Deluge"
        }

-- Helper functions

ensureLoggedIn :: DelugeClient -> IO ()
ensureLoggedIn DelugeClient{..} = do
  jar <- IORef.readIORef delCookieJar
  case jar of
    Just _ -> pure ()  -- Already logged in
    Nothing -> do
      -- Login to Deluge web UI
      let url = T.unpack delUrl <> "/json"
          manager = getManager delHttpClient
      
      reqId <- IORef.atomicModifyIORef' delRequestId (\i -> (i + 1, i))
      let loginReq = JsonRpcRequest "auth.login" [Aeson.toJSON delPassword] reqId
      
      request <- parseRequest url
      let request' = request
            { method = "POST"
            , requestBody = RequestBodyLBS $ Aeson.encode loginReq
            , requestHeaders = [(hContentType, "application/json")]
            }
      
      response <- httpLbs request' manager
      let status = statusCode $ responseStatus response
          newJar = responseCookieJar response
          body = responseBody response
      
      if status == 200
        then case Aeson.eitherDecode body :: Either String (JsonRpcResponse Bool) of
          Right JsonRpcResponse { rpcResult = Just True } -> do
            IORef.writeIORef delCookieJar (Just newJar)
            -- Get list of available hosts and connect to the first one
            hostsResult <- callRpcWithJar delUrl delHttpClient delRequestId (Just newJar) "web.get_hosts" []
            case hostsResult of
              Right (Aeson.Array hosts) | not (null (toList hosts)) -> do
                -- Connect to the first available host
                case toList hosts of
                  (Aeson.Array hostInfo : _) -> 
                    case toList hostInfo of
                      (hostId : _) -> void $ callRpcWithJar delUrl delHttpClient delRequestId (Just newJar) "web.connect" [hostId]
                      [] -> pure ()  -- Empty host info
                  _ -> pure ()  -- No hosts configured, might be standalone
              _ -> pure ()  -- Failed to get hosts, continue anyway
          Right JsonRpcResponse { rpcResult = Just False } ->
            fail "Deluge authentication failed: invalid password"
          Right JsonRpcResponse { rpcError = Just err } ->
            fail $ "Deluge authentication error: " <> show err
          Left err ->
            fail $ "Failed to parse Deluge login response: " <> err
          _ -> fail "Unexpected Deluge login response"
        else fail $ "Failed to connect to Deluge: HTTP " <> show status

callRpc :: DelugeClient -> Text -> [Aeson.Value] -> IO (Either Text Aeson.Value)
callRpc DelugeClient{..} rpcMethod params = do
  jar <- IORef.readIORef delCookieJar
  callRpcWithJar delUrl delHttpClient delRequestId jar rpcMethod params

callRpcWithJar :: Text -> HttpClient -> IORef Int -> Maybe CookieJar -> Text -> [Aeson.Value] -> IO (Either Text Aeson.Value)
callRpcWithJar baseUrl httpClient requestIdRef jar rpcMethod params = do
  let url = T.unpack baseUrl <> "/json"
      manager = getManager httpClient
  
  reqId <- IORef.atomicModifyIORef' requestIdRef (\i -> (i + 1, i))
  let rpcReq = JsonRpcRequest rpcMethod params reqId
  
  request <- parseRequest url
  let request' = request
        { method = "POST"
        , requestBody = RequestBodyLBS $ Aeson.encode rpcReq
        , requestHeaders = [(hContentType, "application/json")]
        , cookieJar = jar
        }
  
  response <- httpLbs request' manager
  let status = statusCode $ responseStatus response
      body = responseBody response
  
  if status == 200
    then case Aeson.eitherDecode body :: Either String (JsonRpcResponse Aeson.Value) of
      Right JsonRpcResponse { rpcResult = Just result } -> pure $ Right result
      Right JsonRpcResponse { rpcError = Just err } -> pure $ Left $ "RPC error: " <> T.pack (show err)
      Left err -> pure $ Left $ "Failed to parse response: " <> T.pack err
      _ -> pure $ Left "Empty RPC response"
    else pure $ Left $ "HTTP error: " <> T.pack (show status)

getAllTorrentsInternal :: DelugeClient -> IO [DelugeTorrent]
getAllTorrentsInternal client = do
  -- Request torrent list with specific fields
  let fields = Aeson.toJSON (["hash", "name", "state", "progress", "total_size", 
                              "total_done", "ratio", "num_seeds", "num_peers", 
                              "save_path", "message"] :: [Text])
      filterDict = object []  -- Empty filter = all torrents
  
  result <- callRpc client "web.update_ui" [fields, filterDict]
  
  case result of
    Left err -> fail $ T.unpack err
    Right val -> case Aeson.fromJSON val of
      Aeson.Success updateResult -> do
        -- update_ui returns {"torrents": {hash: torrentInfo, ...}, ...}
        case lookupKey "torrents" updateResult of
          Just torrentsVal -> case Aeson.fromJSON torrentsVal of
            Aeson.Success torrentsMap -> do
              -- Convert map to list of torrents with hash included
              let torrents = mapToTorrentList torrentsMap
              pure torrents
            _ -> pure []
          Nothing -> pure []
      Aeson.Error _ -> pure []

-- Helper to extract torrents from the map structure
mapToTorrentList :: Aeson.Object -> [DelugeTorrent]
mapToTorrentList obj = 
  [ torrent { dtHash = K.toText key }
  | (key, val) <- KM.toList obj
  , Just torrent <- [parseTorrent val]
  ]
  where
    parseTorrent v = case Aeson.fromJSON v of
      Aeson.Success t -> Just (t :: DelugeTorrent)
      _ -> Nothing

lookupKey :: Text -> Aeson.Value -> Maybe Aeson.Value
lookupKey key (Aeson.Object obj) = KM.lookup (K.fromText key) obj
lookupKey _ _ = Nothing

delugeTorrentToDownloadInfo :: DelugeTorrent -> DownloadInfo
delugeTorrentToDownloadInfo DelugeTorrent{..} =
  let status = case T.toLower dtState of
        "downloading" -> DSDownloading
        "seeding" -> DSCompleted
        "paused" -> DSPaused
        "queued" -> DSQueued
        "checking" -> DSDownloading
        "error" -> DSFailed
        _ -> DSQueued
      -- Combine save_path + torrent name to get full download path
      fullPath = case dtSavePath of
        Just savePath -> 
          let cleanPath = if T.isSuffixOf "/" savePath then savePath else savePath <> "/"
          in Just $ cleanPath <> dtName
        Nothing -> Nothing
  in DownloadInfo
    { diClientId = dtHash
    , diName = Just dtName  -- Torrent name for matching
    , diStatus = status
    , diProgress = dtProgress / 100.0  -- Deluge returns 0-100
    , diDownloadPath = fullPath  -- Full path: save_path + torrent name
    , diErrorMessage = if status == DSFailed then dtMessage else Nothing
    , diSizeBytes = dtTotalSize
    , diDownloadedBytes = dtTotalDone
    , diUploadRatio = dtRatio
    , diSeeders = dtNumSeeds
    , diPeers = dtNumPeers
    , diCategory = Nothing
    }

-- | Extract a filename from a URL, or generate a default one
extractFilename :: Text -> Text
extractFilename url = 
  -- Try to get filename from 'file' query parameter first
  case T.breakOn "file=" url of
    (_, rest) | not (T.null rest) -> 
      let afterFile = T.drop 5 rest  -- Drop "file="
          filename = T.takeWhile (\c -> c /= '&' && c /= ' ') afterFile
      in if T.null filename 
         then defaultFilename
         else T.replace "+" " " filename <> ".torrent"
    _ -> defaultFilename
  where
    defaultFilename = "download.torrent"
