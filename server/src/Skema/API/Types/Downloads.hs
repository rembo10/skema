{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Downloads API types.
module Skema.API.Types.Downloads
  ( DownloadsAPI
  , DownloadResponse(..)
  , DownloadsPagination(..)
  , DownloadsResponse(..)
  , QueueDownloadRequest(..)
  , SlskdFileRequest(..)
  , QueueDownloadResponse(..)
  , DownloadTaskRequest(..)
  ) where

import Skema.API.Types.Tasks (TaskResponse)
import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Downloads API endpoints.
type DownloadsAPI = "downloads" :> Header "Authorization" Text :>
  ( "tasks" :> ReqBody '[JSON] DownloadTaskRequest :> PostCreated '[JSON] TaskResponse
  :<|> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] DownloadsResponse
  :<|> Capture "downloadId" Int64 :> Get '[JSON] DownloadResponse
  :<|> "queue" :> ReqBody '[JSON] QueueDownloadRequest :> PostCreated '[JSON] QueueDownloadResponse
  :<|> Capture "downloadId" Int64 :> DeleteNoContent
  )

-- | Download response.
data DownloadResponse = DownloadResponse
  { downloadResponseId :: Int64
  , downloadResponseCatalogAlbumId :: Int64
  , downloadResponseIndexerName :: Text
  , downloadResponseDownloadUrl :: Text
  , downloadResponseDownloadClient :: Maybe Text
  , downloadResponseDownloadClientId :: Maybe Text
  , downloadResponseStatus :: Text
  , downloadResponseDownloadPath :: Maybe Text
  , downloadResponseTitle :: Text
  , downloadResponseSizeBytes :: Maybe Int64
  , downloadResponseQuality :: Maybe Text
  , downloadResponseFormat :: Maybe Text
  , downloadResponseSeeders :: Maybe Int
  , downloadResponseProgress :: Double
  , downloadResponseErrorMessage :: Maybe Text
  , downloadResponseQueuedAt :: Maybe Text
  , downloadResponseStartedAt :: Maybe Text
  , downloadResponseCompletedAt :: Maybe Text
  , downloadResponseImportedAt :: Maybe Text
  , downloadResponseLibraryPath :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON DownloadResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

instance FromJSON DownloadResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

-- | Pagination info for downloads.
data DownloadsPagination = DownloadsPagination
  { downloadsPaginationTotal :: Int
  , downloadsPaginationOffset :: Int
  , downloadsPaginationLimit :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DownloadsPagination where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance FromJSON DownloadsPagination where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

-- | Paginated downloads response.
data DownloadsResponse = DownloadsResponse
  { downloadsResponsePagination :: DownloadsPagination
  , downloadsResponseDownloads :: [DownloadResponse]
  } deriving (Show, Eq, Generic)

instance ToJSON DownloadsResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }

instance FromJSON DownloadsResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }

-- | Request to queue a new download.
data QueueDownloadRequest = QueueDownloadRequest
  { queueDownloadCatalogAlbumId :: Int64
    -- ^ ID of catalog album to download
  , queueDownloadIndexerName :: Text
    -- ^ Name of indexer where release was found
  , queueDownloadUrl :: Text
    -- ^ Download URL (.nzb or .torrent)
  , queueDownloadTitle :: Text
    -- ^ Release title
  , queueDownloadSizeBytes :: Maybe Int64
  , queueDownloadQuality :: Maybe Text
  , queueDownloadFormat :: Maybe Text
  , queueDownloadSeeders :: Maybe Int
  , queueDownloadSlskdUsername :: Maybe Text
    -- ^ For slskd downloads: username to download from
  , queueDownloadSlskdFiles :: Maybe [SlskdFileRequest]
    -- ^ For slskd downloads: files to download
  } deriving (Show, Eq, Generic)

-- | Slskd file in download request
data SlskdFileRequest = SlskdFileRequest
  { slskdFileRequestFilename :: Text
  , slskdFileRequestSize :: Integer
  } deriving (Show, Eq, Generic)

instance ToJSON SlskdFileRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

instance FromJSON SlskdFileRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

instance ToJSON QueueDownloadRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

instance FromJSON QueueDownloadRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

-- | Response when queuing a download.
data QueueDownloadResponse = QueueDownloadResponse
  { queueDownloadResponseId :: Int64
  , queueDownloadResponseSuccess :: Bool
  , queueDownloadResponseMessage :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON QueueDownloadResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance FromJSON QueueDownloadResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

-- | Request to create a download task.
data DownloadTaskRequest = DownloadTaskRequest
  { downloadTaskType :: Text
    -- ^ Task type: "reidentify"
  , downloadTaskDownloadId :: Int64
    -- ^ Download ID to operate on
  } deriving (Show, Eq, Generic)

instance ToJSON DownloadTaskRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON DownloadTaskRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }
