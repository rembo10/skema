{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skema.DownloadClient.NZBGet
  ( NZBGetClient(..)
  , createNZBGetClient
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), withObject, object)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics ()
import Network.HTTP.Types.URI (urlEncode)

import Skema.DownloadClient.Types
import Skema.HTTP.Client (HttpClient, postJSON)

-- | NZBGet client configuration
data NZBGetClient = NZBGetClient
  { nzbUrl :: Text
  , nzbUsername :: Text
  , nzbPassword :: Text
  , nzbHttpClient :: HttpClient
  , nzbCategory :: Maybe Text
    -- ^ Default category to use (e.g., "music")
  } deriving (Generic)

-- | Create a new NZBGet client
createNZBGetClient :: Text -> Text -> Text -> HttpClient -> Maybe Text -> NZBGetClient
createNZBGetClient = NZBGetClient

-- NZBGet JSON-RPC types
data NZBRPCRequest = NZBRPCRequest
  { rpcMethod :: Text
  , rpcParams :: [Aeson.Value]
  , rpcId :: Int
  } deriving (Show, Generic)

data NZBRPCResponse a = NZBRPCResponse
  { rpcResult :: Maybe a
  , rpcError :: Maybe NZBRPCError
  , rpcResponseId :: Int
  } deriving (Show, Generic)

data NZBRPCError = NZBRPCError
  { rpcErrorCode :: Int
  , rpcErrorMessage :: Text
  } deriving (Show, Generic)

-- NZBGet group (download) info
data NZBGroup = NZBGroup
  { nzbGroupId :: Int
  , nzbGroupName :: Text
  , nzbGroupStatus :: Text
  , nzbGroupFileSizeMB :: Int
  , nzbGroupRemainingSizeMB :: Int
  , nzbGroupDownloadedSizeMB :: Int
  , nzbGroupCategory :: Text
  } deriving (Show, Generic)

-- NZBGet history item
data NZBHistoryItem = NZBHistoryItem
  { nzbHistId :: Int
  , nzbHistName :: Text
  , nzbHistStatus :: Text
  , nzbHistFileSizeMB :: Int
  , nzbHistCategory :: Text
  , nzbHistDestDir :: Maybe Text
  , nzbHistFinalDir :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON NZBRPCRequest where
  toJSON NZBRPCRequest{..} = object
    [ "method" .= rpcMethod
    , "params" .= rpcParams
    , "id" .= rpcId
    , "jsonrpc" .= ("2.0" :: Text)
    ]

instance FromJSON a => FromJSON (NZBRPCResponse a) where
  parseJSON = withObject "NZBRPCResponse" $ \v -> do
    rpcResult <- v .:? "result"
    rpcError <- v .:? "error"
    rpcResponseId <- v .: "id"
    pure NZBRPCResponse{..}

instance FromJSON NZBRPCError where
  parseJSON = withObject "NZBRPCError" $ \v -> do
    rpcErrorCode <- v .: "code"
    rpcErrorMessage <- v .: "message"
    pure NZBRPCError{..}

instance FromJSON NZBGroup where
  parseJSON = withObject "NZBGroup" $ \v -> do
    nzbGroupId <- v .: "NZBID"
    nzbGroupName <- v .: "NZBName"
    nzbGroupStatus <- v .: "Status"
    nzbGroupFileSizeMB <- v .: "FileSizeMB"
    nzbGroupRemainingSizeMB <- v .: "RemainingSizeMB"
    nzbGroupDownloadedSizeMB <- v .: "DownloadedSizeMB"
    nzbGroupCategory <- v .: "Category"
    pure NZBGroup{..}

instance FromJSON NZBHistoryItem where
  parseJSON = withObject "NZBHistoryItem" $ \v -> do
    nzbHistId <- v .: "NZBID"
    nzbHistName <- v .: "Name"
    nzbHistStatus <- v .: "Status"
    nzbHistFileSizeMB <- v .: "FileSizeMB"
    nzbHistCategory <- v .: "Category"
    nzbHistDestDir <- v .:? "DestDir"
    nzbHistFinalDir <- v .:? "FinalDir"
    pure NZBHistoryItem{..}

instance DownloadClientAPI NZBGetClient where
  testConnection client = do
    result <- try $ makeRPCRequest client "version" [] :: IO (Either SomeException (NZBRPCResponse Text))
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right response ->
        case rpcError response of
          Just err -> pure $ Left $ rpcErrorMessage err
          Nothing -> pure $ Right ()

  addDownload client AddDownloadRequest{..} = do
    let url = adrUrl
        name = adrTitle
        category = fromMaybe "" $ adrCategory <|> nzbCategory client
        priority = fromMaybe 0 adrPriority
        addToTop = False
        addPaused = False
        dupeKey = ""
        dupeScore = 0
        dupeMode = "SCORE"
        params =
          [ Aeson.String name  -- NZBFilename
          , Aeson.String url   -- Content (URL)
          , Aeson.String category
          , Aeson.Number (fromIntegral priority)
          , Aeson.Bool addToTop
          , Aeson.Bool addPaused
          , Aeson.String dupeKey
          , Aeson.Number (fromIntegral dupeScore)
          , Aeson.String dupeMode
          , Aeson.Array mempty  -- PPParameters
          ]

    result <- try $ makeRPCRequest client "append" params :: IO (Either SomeException (NZBRPCResponse Int))
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right response ->
        case (rpcError response, rpcResult response) of
          (Just err, _) -> pure $ Left $ rpcErrorMessage err
          (Nothing, Just nzbId) | nzbId > 0 -> pure $ Right $ AddDownloadResult
            { adrClientId = T.pack $ show nzbId
            , adrSuccess = True
            , adrMessage = Nothing
            }
          _ -> pure $ Left "NZBGet returned an error code"

  getDownloadStatus client downloadId = do
    -- First check queue
    let nzbId = readMaybe (T.unpack downloadId) :: Maybe Int
    case nzbId of
      Nothing -> pure $ Left "Invalid download ID"
      Just _ -> do
        -- Get all groups (queue items)
        groupsResult <- try $ makeRPCRequest client "listgroups" [Aeson.Number 0] :: IO (Either SomeException (NZBRPCResponse [NZBGroup]))
        case groupsResult of
          Right groupsResp | Just groups <- rpcResult groupsResp -> do
            case find (\g -> T.pack (show $ nzbGroupId g) == downloadId) groups of
              Just group -> pure $ Right $ groupToDownloadInfo group
              Nothing -> do
                -- Not in queue, check history
                histResult <- try $ makeRPCRequest client "history" [Aeson.Bool False] :: IO (Either SomeException (NZBRPCResponse [NZBHistoryItem]))
                case histResult of
                  Right histResp | Just items <- rpcResult histResp ->
                    case find (\h -> T.pack (show $ nzbHistId h) == downloadId) items of
                      Just item -> pure $ Right $ historyToDownloadInfo item
                      Nothing -> pure $ Left "Download not found"
                  Right _ -> pure $ Left "No history result"
                  Left err -> pure $ Left $ T.pack $ show err
          Right _ -> pure $ Left "No groups result"
          Left err -> pure $ Left $ T.pack $ show err

  getAllDownloads client = do
    groupsResult <- try $ makeRPCRequest client "listgroups" [Aeson.Number 0] :: IO (Either SomeException (NZBRPCResponse [NZBGroup]))
    histResult <- try $ makeRPCRequest client "history" [Aeson.Bool False] :: IO (Either SomeException (NZBRPCResponse [NZBHistoryItem]))

    case (groupsResult, histResult) of
      (Right groupsResp, Right histResp) ->
        let groups = maybe [] id (rpcResult groupsResp)
            history = maybe [] id (rpcResult histResp)
            queueDownloads = map groupToDownloadInfo groups
            histDownloads = map historyToDownloadInfo history
        in pure $ Right $ queueDownloads <> histDownloads
      (Left err, _) -> pure $ Left $ T.pack $ show err
      (_, Left err) -> pure $ Left $ T.pack $ show err

  pauseDownload client downloadId = do
    let nzbId = readMaybe (T.unpack downloadId) :: Maybe Int
    case nzbId of
      Nothing -> pure $ Left "Invalid download ID"
      Just nid -> do
        result <- try $ makeRPCRequest client "editqueue" [Aeson.String "GroupPause", Aeson.Number 0, Aeson.String "", Aeson.Array $ fromList [Aeson.Number $ fromIntegral nid]] :: IO (Either SomeException (NZBRPCResponse Bool))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right response ->
            case rpcError response of
              Just err -> pure $ Left $ rpcErrorMessage err
              Nothing -> pure $ Right ()

  resumeDownload client downloadId = do
    let nzbId = readMaybe (T.unpack downloadId) :: Maybe Int
    case nzbId of
      Nothing -> pure $ Left "Invalid download ID"
      Just nid -> do
        result <- try $ makeRPCRequest client "editqueue" [Aeson.String "GroupResume", Aeson.Number 0, Aeson.String "", Aeson.Array $ fromList [Aeson.Number $ fromIntegral nid]] :: IO (Either SomeException (NZBRPCResponse Bool))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right response ->
            case rpcError response of
              Just err -> pure $ Left $ rpcErrorMessage err
              Nothing -> pure $ Right ()

  removeDownload client downloadId deleteFiles = do
    let nzbId = readMaybe (T.unpack downloadId) :: Maybe Int
    case nzbId of
      Nothing -> pure $ Left "Invalid download ID"
      Just nid -> do
        let action = if deleteFiles then "GroupFinalDelete" else "GroupDelete"
        result <- try $ makeRPCRequest client "editqueue" [Aeson.String action, Aeson.Number 0, Aeson.String "", Aeson.Array $ fromList [Aeson.Number $ fromIntegral nid]] :: IO (Either SomeException (NZBRPCResponse Bool))
        case result of
          Left err -> pure $ Left $ T.pack $ show err
          Right response ->
            case rpcError response of
              Just err -> pure $ Left $ rpcErrorMessage err
              Nothing -> pure $ Right ()

-- Helper functions

makeRPCRequest :: FromJSON a => NZBGetClient -> Text -> [Aeson.Value] -> IO (NZBRPCResponse a)
makeRPCRequest NZBGetClient{..} method params = do
  let baseUrl = if T.isSuffixOf "/" nzbUrl then T.dropEnd 1 nzbUrl else nzbUrl
      -- NZBGet JSON-RPC endpoint with HTTP basic auth in URL
      url = if T.isInfixOf "@" baseUrl
              then baseUrl <> "/jsonrpc"  -- Auth already in URL
              else
                -- Add auth to URL: http://username:password@host:port/jsonrpc
                let (proto, rest) = T.breakOn "://" baseUrl
                    rest' = T.drop 3 rest  -- Remove ://
                in proto <> "://" <> nzbUsername <> ":" <> nzbPassword <> "@" <> rest' <> "/jsonrpc"

      request = NZBRPCRequest method params 1

  result <- postJSON nzbHttpClient url request
  case result of
    Left err -> fail $ "NZBGet HTTP error: " <> show err
    Right val -> pure val

groupToDownloadInfo :: NZBGroup -> DownloadInfo
groupToDownloadInfo NZBGroup{..} =
  let status = case T.toUpper nzbGroupStatus of
        "DOWNLOADING" -> DSDownloading
        "PAUSED" -> DSPaused
        "QUEUED" -> DSQueued
        "FETCHING" -> DSDownloading
        "PP_QUEUED" -> DSDownloading
        "LOADING_PARS" -> DSDownloading
        "VERIFYING_SOURCES" -> DSDownloading
        "REPAIRING" -> DSDownloading
        "VERIFYING_REPAIRED" -> DSDownloading
        "RENAMING" -> DSDownloading
        "UNPACKING" -> DSDownloading
        "MOVING" -> DSDownloading
        "EXECUTING_SCRIPT" -> DSDownloading
        "PP_FINISHED" -> DSCompleted
        _ -> DSQueued
      sizeBytes = Just $ fromIntegral nzbGroupFileSizeMB * 1024 * 1024
      downloadedBytes = Just $ fromIntegral nzbGroupDownloadedSizeMB * 1024 * 1024
      remainingBytes = fromIntegral nzbGroupRemainingSizeMB * 1024 * 1024
      progress = if nzbGroupFileSizeMB > 0
                   then fromIntegral (nzbGroupFileSizeMB - nzbGroupRemainingSizeMB) / fromIntegral nzbGroupFileSizeMB
                   else 0.0
  in DownloadInfo
    { diClientId = T.pack $ show nzbGroupId
    , diStatus = status
    , diProgress = progress
    , diDownloadPath = Nothing  -- NZBGet doesn't provide download path in listgroups
    , diErrorMessage = Nothing
    , diSizeBytes = sizeBytes
    , diDownloadedBytes = downloadedBytes
    , diUploadRatio = Nothing
    , diSeeders = Nothing
    , diPeers = Nothing
    , diCategory = if T.null nzbGroupCategory then Nothing else Just nzbGroupCategory
    }

historyToDownloadInfo :: NZBHistoryItem -> DownloadInfo
historyToDownloadInfo NZBHistoryItem{..} =
  let status = case T.toUpper nzbHistStatus of
        "SUCCESS/ALL" -> DSCompleted
        "SUCCESS/UNPACK" -> DSCompleted
        "SUCCESS/PAR" -> DSCompleted
        "SUCCESS/HEALTH" -> DSCompleted
        "FAILURE/HEALTH" -> DSFailed
        "FAILURE/PAR" -> DSFailed
        "FAILURE/UNPACK" -> DSFailed
        "FAILURE/MOVE" -> DSFailed
        "FAILURE/SCRIPT" -> DSFailed
        "DELETED/MANUAL" -> DSFailed
        "DELETED/DUPE" -> DSFailed
        _ -> if T.isPrefixOf "SUCCESS" nzbHistStatus then DSCompleted else DSFailed
      sizeBytes = Just $ fromIntegral nzbHistFileSizeMB * 1024 * 1024
      downloadPath = nzbHistFinalDir <|> nzbHistDestDir
  in DownloadInfo
    { diClientId = T.pack $ show nzbHistId
    , diStatus = status
    , diProgress = if status == DSCompleted then 1.0 else 0.0
    , diDownloadPath = downloadPath
    , diErrorMessage = if status == DSFailed then Just nzbHistStatus else Nothing
    , diSizeBytes = sizeBytes
    , diDownloadedBytes = sizeBytes
    , diUploadRatio = Nothing
    , diSeeders = Nothing
    , diPeers = Nothing
    , diCategory = if T.null nzbHistCategory then Nothing else Just nzbHistCategory
    }
