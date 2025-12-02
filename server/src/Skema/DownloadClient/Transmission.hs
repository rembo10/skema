{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skema.DownloadClient.Transmission
  ( TransmissionClient(..)
  , createTransmissionClient
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Data.IORef ()
import Data.List (lookup)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics ()
import Network.HTTP.Types (statusCode, hContentType)

import Skema.DownloadClient.Types
import Skema.HTTP.Client (HttpClient, getManager)
import Network.HTTP.Client (RequestBody(..), parseRequest, httpLbs, responseBody, responseStatus, responseHeaders, method, requestBody, requestHeaders)

-- | Transmission client configuration
data TransmissionClient = TransmissionClient
  { transUrl :: Text
  , transUsername :: Maybe Text
  , transPassword :: Maybe Text
  , transHttpClient :: HttpClient
  , transSessionId :: IORef (Maybe Text)
  }

-- | Create a new Transmission client
createTransmissionClient :: Text -> Maybe Text -> Maybe Text -> HttpClient -> IO TransmissionClient
createTransmissionClient url username password httpClient = do
  sessionIdRef <- IORef.newIORef Nothing
  pure $ TransmissionClient url username password httpClient sessionIdRef

-- Transmission JSON-RPC types
data TransRPCRequest = TransRPCRequest
  { rpcMethod :: Text
  , rpcArguments :: Aeson.Value
  } deriving (Show, Generic)

data TransRPCResponse a = TransRPCResponse
  { rpcResult :: Text
  , rpcResultData :: Maybe a
  } deriving (Show, Generic)

data TransTorrent = TransTorrent
  { ttId :: Int
  , ttHashString :: Text
  , ttName :: Text
  , ttStatus :: Int
  , ttPercentDone :: Double
  , ttTotalSize :: Maybe Integer
  , ttDownloadedEver :: Maybe Integer
  , ttUploadRatio :: Maybe Double
  , ttError :: Int
  , ttErrorString :: Maybe Text
  , ttDownloadDir :: Maybe Text
  , ttPeersConnected :: Maybe Int
  , ttPeersSendingToUs :: Maybe Int
  } deriving (Show, Generic)

data TransAddResult = TransAddResult
  { tarTorrentAdded :: Maybe TransAddedTorrent
  , tarTorrentDuplicate :: Maybe TransAddedTorrent
  } deriving (Show, Generic)

data TransAddedTorrent = TransAddedTorrent
  { tatId :: Int
  , tatHashString :: Text
  , tatName :: Text
  } deriving (Show, Generic)

instance ToJSON TransRPCRequest where
  toJSON TransRPCRequest{..} = object
    [ "method" .= rpcMethod
    , "arguments" .= rpcArguments
    ]

instance FromJSON a => FromJSON (TransRPCResponse a) where
  parseJSON = withObject "TransRPCResponse" $ \v -> do
    rpcResult <- v .: "result"
    rpcResultData <- v .:? "arguments"
    pure TransRPCResponse{..}

instance FromJSON TransTorrent where
  parseJSON = withObject "TransTorrent" $ \v -> do
    ttId <- v .: "id"
    ttHashString <- v .: "hashString"
    ttName <- v .: "name"
    ttStatus <- v .: "status"
    ttPercentDone <- v .: "percentDone"
    ttTotalSize <- v .:? "totalSize"
    ttDownloadedEver <- v .:? "downloadedEver"
    ttUploadRatio <- v .:? "uploadRatio"
    ttError <- v .: "error"
    ttErrorString <- v .:? "errorString"
    ttDownloadDir <- v .:? "downloadDir"
    ttPeersConnected <- v .:? "peersConnected"
    ttPeersSendingToUs <- v .:? "peersSendingToUs"
    pure TransTorrent{..}

instance FromJSON TransAddResult where
  parseJSON = withObject "TransAddResult" $ \v -> do
    tarTorrentAdded <- v .:? "torrent-added"
    tarTorrentDuplicate <- v .:? "torrent-duplicate"
    pure TransAddResult{..}

instance FromJSON TransAddedTorrent where
  parseJSON = withObject "TransAddedTorrent" $ \v -> do
    tatId <- v .: "id"
    tatHashString <- v .: "hashString"
    tatName <- v .: "name"
    pure TransAddedTorrent{..}

instance DownloadClientAPI TransmissionClient where
  testConnection client = do
    result <- try $ rpcCall client "session-get" (object []) :: IO (Either SomeException (TransRPCResponse Aeson.Value))
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right response ->
        if rpcResult response == "success"
          then pure $ Right ()
          else pure $ Left $ "Transmission error: " <> rpcResult response

  addDownload client AddDownloadRequest{..} = do
    let args = object
          [ "filename" .= adrUrl
          , "paused" .= False
          ]
    result <- try $ rpcCall client "torrent-add" args :: IO (Either SomeException (TransRPCResponse TransAddResult))
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right response ->
        case rpcResultData response of
          Just addResult ->
            case tarTorrentAdded addResult <|> tarTorrentDuplicate addResult of
              Just torrent -> pure $ Right $ AddDownloadResult
                { adrClientId = T.pack $ show $ tatId torrent
                , adrSuccess = True
                , adrMessage = Nothing
                }
              Nothing -> pure $ Left "Transmission did not return torrent info"
          Nothing -> pure $ Left "No result data from Transmission"

  getDownloadStatus client downloadId = do
    case readMaybe (T.unpack downloadId) of
      Nothing -> pure $ Left "Invalid torrent ID"
      Just torrentId -> do
        let args = object
              [ "ids" .= ([torrentId] :: [Int])
              , "fields" .= (["id", "hashString", "name", "status", "percentDone",
                             "totalSize", "downloadedEver", "uploadRatio", "error",
                             "errorString", "downloadDir", "peersConnected", "peersSendingToUs"] :: [Text])
              ]
        result <- try $ rpcCall client "torrent-get" args :: IO (Either SomeException (TransRPCResponse Aeson.Value))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right response ->
            case rpcResultData response of
              Just resultData ->
                case Aeson.fromJSON resultData of
                  Aeson.Success torrentsObj -> do
                    case Aeson.fromJSON torrentsObj of
                      Aeson.Success (torrents :: [TransTorrent]) ->
                        case torrents of
                          (torrent:_) -> pure $ Right $ torrentToDownloadInfo torrent
                          [] -> pure $ Left "Torrent not found"
                      Aeson.Error err -> pure $ Left $ T.pack err
                  Aeson.Error err -> pure $ Left $ T.pack err
              Nothing -> pure $ Left "No torrents in response"

  getAllDownloads client = do
    let args = object
          [ "fields" .= (["id", "hashString", "name", "status", "percentDone",
                         "totalSize", "downloadedEver", "uploadRatio", "error",
                         "errorString", "downloadDir", "peersConnected", "peersSendingToUs"] :: [Text])
          ]
    result <- try $ rpcCall client "torrent-get" args :: IO (Either SomeException (TransRPCResponse Aeson.Value))
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right response ->
        case rpcResultData response of
          Just resultData ->
            case Aeson.fromJSON resultData of
              Aeson.Success torrentsObj ->
                case Aeson.fromJSON torrentsObj of
                  Aeson.Success (torrents :: [TransTorrent]) ->
                    pure $ Right $ map torrentToDownloadInfo torrents
                  Aeson.Error err -> pure $ Left $ T.pack err
              Aeson.Error err -> pure $ Left $ T.pack err
          Nothing -> pure $ Left "No torrents in response"

  pauseDownload client downloadId = do
    case readMaybe (T.unpack downloadId) of
      Nothing -> pure $ Left "Invalid torrent ID"
      Just torrentId -> do
        let args = object ["ids" .= ([torrentId] :: [Int])]
        result <- try $ rpcCall client "torrent-stop" args :: IO (Either SomeException (TransRPCResponse Aeson.Value))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right _ -> pure $ Right ()

  resumeDownload client downloadId = do
    case readMaybe (T.unpack downloadId) of
      Nothing -> pure $ Left "Invalid torrent ID"
      Just torrentId -> do
        let args = object ["ids" .= ([torrentId] :: [Int])]
        result <- try $ rpcCall client "torrent-start" args :: IO (Either SomeException (TransRPCResponse Aeson.Value))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right _ -> pure $ Right ()

  removeDownload client downloadId deleteFiles = do
    case readMaybe (T.unpack downloadId) of
      Nothing -> pure $ Left "Invalid torrent ID"
      Just torrentId -> do
        let args = object
              [ "ids" .= ([torrentId] :: [Int])
              , "delete-local-data" .= deleteFiles
              ]
        result <- try $ rpcCall client "torrent-remove" args :: IO (Either SomeException (TransRPCResponse Aeson.Value))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right _ -> pure $ Right ()

-- Helper functions

rpcCall :: FromJSON a => TransmissionClient -> Text -> Aeson.Value -> IO (TransRPCResponse a)
rpcCall client@TransmissionClient{..} method args = do
  -- Get or fetch session ID
  sessionId <- IORef.readIORef transSessionId
  result <- rpcCallWithSession client method args sessionId
  case result of
    Left newSessionId -> do
      -- Got 409, retry with new session ID
      IORef.writeIORef transSessionId (Just newSessionId)
      result2 <- rpcCallWithSession client method args (Just newSessionId)
      case result2 of
        Right response -> pure response
        Left _ -> fail "Failed to get valid session ID"
    Right response -> pure response

rpcCallWithSession :: FromJSON a => TransmissionClient -> Text -> Aeson.Value -> Maybe Text -> IO (Either Text (TransRPCResponse a))
rpcCallWithSession TransmissionClient{..} method args sessionId = do
  let rpcReq = TransRPCRequest method args
      body = Aeson.encode rpcReq
      -- Get the raw manager for special session handling
      manager = getManager transHttpClient

  request <- parseRequest $ T.unpack transUrl <> "/transmission/rpc"
  let request' = request
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ (hContentType, "application/json")
            ] <> maybe [] (\sid -> [("X-Transmission-Session-Id", TE.encodeUtf8 sid)]) sessionId
        }

  response <- httpLbs request' manager
  let status = statusCode $ responseStatus response

  if status == 409
    then do
      -- Extract session ID from response headers
      let headers = responseHeaders response
          sessionIdHeader = lookup "X-Transmission-Session-Id" headers
      case sessionIdHeader of
        Just sid -> pure $ Left $ TE.decodeUtf8 sid
        Nothing -> fail "Got 409 but no session ID in response"
    else do
      let responseBody' = responseBody response
      case Aeson.eitherDecode responseBody' of
        Left err -> fail $ "Failed to parse Transmission response: " <> err
        Right result -> pure $ Right result

torrentToDownloadInfo :: TransTorrent -> DownloadInfo
torrentToDownloadInfo TransTorrent{..} =
  let status = case ttStatus of
        0 -> DSPaused     -- Stopped
        1 -> DSQueued     -- Check wait
        2 -> DSQueued     -- Check
        3 -> DSQueued     -- Download wait
        4 -> DSDownloading -- Downloading
        5 -> DSQueued     -- Seed wait
        6 -> DSCompleted  -- Seeding
        _ -> DSQueued
      errorMsg = if ttError /= 0 then ttErrorString else Nothing
  in DownloadInfo
    { diClientId = T.pack $ show ttId
    , diName = Just ttName  -- Torrent name for matching
    , diStatus = status
    , diProgress = ttPercentDone
    , diDownloadPath = ttDownloadDir
    , diErrorMessage = errorMsg
    , diSizeBytes = ttTotalSize
    , diDownloadedBytes = ttDownloadedEver
    , diUploadRatio = ttUploadRatio
    , diSeeders = ttPeersSendingToUs
    , diPeers = ttPeersConnected
    , diCategory = Nothing  -- Transmission doesn't support categories
    }
