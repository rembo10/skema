{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skema.DownloadClient.SABnzbd
  ( SABnzbdClient(..)
  , createSABnzbdClient
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import GHC.Generics ()
import Network.HTTP.Types.URI (urlEncode)
import Text.Read ()

import Skema.DownloadClient.Types
import Skema.HTTP.Client (HttpClient, getJSON)

-- | SABnzbd client configuration
data SABnzbdClient = SABnzbdClient
  { sabUrl :: Text
  , sabApiKey :: Text
  , sabHttpClient :: HttpClient
  , sabDownloadDir :: Maybe Text
    -- ^ Base download directory (e.g., "/mnt/stashbox/downloads/tmp/")
  , sabCategory :: Maybe Text
    -- ^ Category to use (e.g., "music" or "headphones")
  } deriving (Generic)

-- | Create a new SABnzbd client
createSABnzbdClient :: Text -> Text -> HttpClient -> Maybe Text -> Maybe Text -> SABnzbdClient
createSABnzbdClient = SABnzbdClient

-- SABnzbd API response types
data SABResponse a = SABResponse
  { sabStatus :: Bool
  , sabResult :: Maybe a
  , sabError :: Maybe Text
  } deriving (Show, Generic)

data SABAddResponse = SABAddResponse
  { sabAddNzoIds :: [Text]
  } deriving (Show, Generic)

data SABQueueResponse = SABQueueResponse
  { sabQueueSlots :: [SABQueueSlot]
  } deriving (Show, Generic)

data SABQueueSlot = SABQueueSlot
  { sabSlotNzoId :: Text
  , sabSlotFilename :: Text
  , sabSlotStatus :: Text
  , sabSlotMbLeft :: Maybe Double
  , sabSlotMb :: Maybe Double
  , sabSlotPercentage :: Maybe Double
  , sabSlotCategory :: Maybe Text
  } deriving (Show, Generic)

data SABHistoryResponse = SABHistoryResponse
  { sabHistorySlots :: [SABHistorySlot]
  } deriving (Show, Generic)

data SABHistorySlot = SABHistorySlot
  { sabHistNzoId :: Text
  , sabHistName :: Text
  , sabHistStatus :: Text
  , sabHistFailMessage :: Maybe Text
  , sabHistBytes :: Maybe Integer
  , sabHistCategory :: Maybe Text
  , sabHistStoragePath :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (SABResponse a) where
  parseJSON = withObject "SABResponse" $ \v -> do
    sabStatus <- v .:? "status" >>= \s -> pure $ maybe False id s
    sabResult <- v .:? "result"
    sabError <- v .:? "error"
    pure SABResponse{..}

instance FromJSON SABAddResponse where
  parseJSON = withObject "SABAddResponse" $ \v ->
    SABAddResponse <$> v .: "nzo_ids"

instance FromJSON SABQueueResponse where
  parseJSON = withObject "SABQueueResponse" $ \v -> do
    queue <- v .: "queue"
    slots <- queue .: "slots"
    pure $ SABQueueResponse slots

instance FromJSON SABQueueSlot where
  parseJSON = withObject "SABQueueSlot" $ \v -> do
    sabSlotNzoId <- v .: "nzo_id"
    sabSlotFilename <- v .: "filename"
    sabSlotStatus <- v .: "status"
    -- SABnzbd returns these as strings, not numbers
    sabSlotMbLeft <- (v .:? "mbleft") >>= parseStringDouble
    sabSlotMb <- (v .:? "mb") >>= parseStringDouble
    sabSlotPercentage <- (v .:? "percentage") >>= parseStringDouble
    sabSlotCategory <- v .:? "cat"
    pure SABQueueSlot{..}

-- | Parse a field that can be either a string-encoded number or an actual JSON number
parseStringDouble :: Maybe Aeson.Value -> Parser (Maybe Double)
parseStringDouble Nothing = pure Nothing
parseStringDouble (Just (Aeson.String s)) = pure $ readMaybe (T.unpack s)
parseStringDouble (Just (Aeson.Number n)) = pure $ Just $ realToFrac n
parseStringDouble _ = pure Nothing

instance FromJSON SABHistoryResponse where
  parseJSON = withObject "SABHistoryResponse" $ \v -> do
    history <- v .: "history"
    slots <- history .: "slots"
    pure $ SABHistoryResponse slots

instance FromJSON SABHistorySlot where
  parseJSON = withObject "SABHistorySlot" $ \v -> do
    sabHistNzoId <- v .: "nzo_id"
    sabHistName <- v .: "name"
    sabHistStatus <- v .: "status"
    sabHistFailMessage <- v .:? "fail_message"
    sabHistBytes <- v .:? "bytes"
    sabHistCategory <- v .:? "category"
    sabHistStoragePath <- v .:? "storage"
    pure SABHistorySlot{..}

instance DownloadClientAPI SABnzbdClient where
  testConnection client = do
    result <- try $ makeRequest client "mode=version" :: IO (Either SomeException Aeson.Value)
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right _ -> pure $ Right ()

  addDownload client AddDownloadRequest{..} = do
    let url = decodeUtf8 $ urlEncode True $ encodeUtf8 adrUrl
        name = decodeUtf8 $ urlEncode True $ encodeUtf8 adrTitle
        cat = maybe "" (\c -> "&cat=" <> decodeUtf8 (urlEncode True $ encodeUtf8 c)) adrCategory
        priority = maybe "" (\p -> "&priority=" <> T.pack (show p)) adrPriority
        params = "mode=addurl&name=" <> url <> "&nzbname=" <> name <> cat <> priority

    result <- try $ makeRequest client params :: IO (Either SomeException SABAddResponse)
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right response ->
        case sabAddNzoIds response of
          (nzoId:_) -> pure $ Right $ AddDownloadResult
            { adrClientId = nzoId
            , adrSuccess = True
            , adrMessage = Nothing
            }
          [] -> pure $ Left "SABnzbd did not return an NZO ID"

  getDownloadStatus client downloadId = do
    -- First check queue
    queueResult <- try $ makeRequest client "mode=queue" :: IO (Either SomeException SABQueueResponse)
    case queueResult of
      Right queueResp -> do
        case find (\slot -> sabSlotNzoId slot == downloadId) (sabQueueSlots queueResp) of
          Just slot -> pure $ Right $ queueSlotToDownloadInfo client slot
          Nothing -> do
            -- Not in queue, check history
            histResult <- try $ makeRequest client "mode=history" :: IO (Either SomeException SABHistoryResponse)
            case histResult of
              Right histResp ->
                case find (\slot -> sabHistNzoId slot == downloadId) (sabHistorySlots histResp) of
                  Just slot -> pure $ Right $ historySlotToDownloadInfo client slot
                  Nothing -> pure $ Left "Download not found"
              Left err -> pure $ Left $ T.pack $ show err
      Left err -> pure $ Left $ T.pack $ show err

  getAllDownloads client = do
    queueResult <- try $ makeRequest client "mode=queue" :: IO (Either SomeException SABQueueResponse)
    histResult <- try $ makeRequest client "mode=history&limit=100" :: IO (Either SomeException SABHistoryResponse)

    case (queueResult, histResult) of
      (Right queueResp, Right histResp) ->
        let queueDownloads = map (queueSlotToDownloadInfo client) (sabQueueSlots queueResp)
            histDownloads = map (historySlotToDownloadInfo client) (sabHistorySlots histResp)
        in pure $ Right $ queueDownloads <> histDownloads
      (Left err, _) -> pure $ Left $ T.pack $ show err
      (_, Left err) -> pure $ Left $ T.pack $ show err

  pauseDownload client downloadId = do
    result <- try $ makeRequest client ("mode=queue&name=pause&value=" <> downloadId) :: IO (Either SomeException Aeson.Value)
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right _ -> pure $ Right ()

  resumeDownload client downloadId = do
    result <- try $ makeRequest client ("mode=queue&name=resume&value=" <> downloadId) :: IO (Either SomeException Aeson.Value)
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right _ -> pure $ Right ()

  removeDownload client downloadId deleteFiles = do
    let mode = if deleteFiles then "delete" else "remove"
    result <- try $ makeRequest client ("mode=queue&name=" <> mode <> "&value=" <> downloadId) :: IO (Either SomeException Aeson.Value)
    case result of
      Left err -> pure $ Left $ T.pack $ show err
      Right _ -> pure $ Right ()

-- Helper functions

makeRequest :: FromJSON a => SABnzbdClient -> Text -> IO a
makeRequest SABnzbdClient{..} params = do
  -- Ensure URL has proper format (remove trailing slash if present)
  let baseUrl = if T.isSuffixOf "/" sabUrl then T.dropEnd 1 sabUrl else sabUrl
      url = baseUrl <> "/api?output=json&apikey=" <> sabApiKey <> "&" <> params
  result <- getJSON sabHttpClient url
  case result of
    Left err -> fail $ "SABnzbd HTTP error: " <> show err
    Right val -> pure val

queueSlotToDownloadInfo :: SABnzbdClient -> SABQueueSlot -> DownloadInfo
queueSlotToDownloadInfo SABnzbdClient{..} SABQueueSlot{..} =
  let status = case T.toLower sabSlotStatus of
        "downloading" -> DSDownloading
        "paused" -> DSPaused
        "queued" -> DSQueued
        _ -> DSQueued
      progress = maybe 0.0 (\p -> p / 100.0) sabSlotPercentage
      sizeBytes = fmap (\mb -> round (mb * 1024 * 1024)) sabSlotMb
      downloadedBytes = case (sabSlotMb, sabSlotMbLeft) of
        (Just total, Just left) -> Just $ round ((total - left) * 1024 * 1024)
        _ -> Nothing
      -- Construct download path from base dir + filename
      -- Note: We don't append category here because SABnzbd already includes it in the actual path,
      -- and the download_dir config should be the full path where downloads go
      downloadPath = case sabDownloadDir of
        Just baseDir ->
          let cleanBase = if T.isSuffixOf "/" baseDir then baseDir else baseDir <> "/"
          in Just $ cleanBase <> sabSlotFilename
        Nothing -> Nothing  -- Can't construct path without base dir
  in DownloadInfo
    { diClientId = sabSlotNzoId
    , diStatus = status
    , diProgress = progress
    , diDownloadPath = downloadPath
    , diErrorMessage = Nothing
    , diSizeBytes = sizeBytes
    , diDownloadedBytes = downloadedBytes
    , diUploadRatio = Nothing
    , diSeeders = Nothing
    , diPeers = Nothing
    , diCategory = sabSlotCategory  -- Category from SABnzbd response
    }

historySlotToDownloadInfo :: SABnzbdClient -> SABHistorySlot -> DownloadInfo
historySlotToDownloadInfo SABnzbdClient{..} SABHistorySlot{..} =
  let status = case T.toLower sabHistStatus of
        "completed" -> DSCompleted
        "failed" -> DSFailed
        -- Post-processing statuses should be treated as still downloading
        "extracting" -> DSDownloading
        "moving" -> DSDownloading
        "running" -> DSDownloading
        -- Unknown statuses default to downloading to be safe
        _ -> DSDownloading
      -- Use storage path from SABnzbd if available, otherwise construct from config
      downloadPath = case sabHistStoragePath of
        Just path | not (T.null path) -> Just path
        _ ->
          -- Construct from base dir + name (SABnzbd already includes category in the actual path)
          case sabDownloadDir of
            Just baseDir ->
              let cleanBase = if T.isSuffixOf "/" baseDir then baseDir else baseDir <> "/"
              in Just $ cleanBase <> sabHistName
            Nothing -> Nothing  -- Can't construct path without base dir
  in DownloadInfo
    { diClientId = sabHistNzoId
    , diStatus = status
    , diProgress = if status == DSCompleted then 1.0 else 0.0
    , diDownloadPath = downloadPath
    , diErrorMessage = sabHistFailMessage
    , diSizeBytes = sabHistBytes
    , diDownloadedBytes = sabHistBytes
    , diUploadRatio = Nothing
    , diSeeders = Nothing
    , diPeers = Nothing
    , diCategory = sabHistCategory  -- Category from SABnzbd response
    }
