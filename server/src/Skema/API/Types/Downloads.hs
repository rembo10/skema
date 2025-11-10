{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Downloads API types.
module Skema.API.Types.Downloads
  ( DownloadsAPI
  , DownloadResponse(..)
  , QueueDownloadRequest(..)
  , QueueDownloadResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Downloads API endpoints.
type DownloadsAPI = "downloads" :> Header "Authorization" Text :>
  ( Get '[JSON] [DownloadResponse]
  :<|> Capture "downloadId" Int64 :> Get '[JSON] DownloadResponse
  :<|> "queue" :> ReqBody '[JSON] QueueDownloadRequest :> PostCreated '[JSON] QueueDownloadResponse
  :<|> Capture "downloadId" Int64 :> "pause" :> Put '[JSON] NoContent
  :<|> Capture "downloadId" Int64 :> "resume" :> Put '[JSON] NoContent
  :<|> Capture "downloadId" Int64 :> QueryParam "deleteFiles" Bool :> DeleteNoContent
  :<|> Capture "downloadId" Int64 :> "reidentify" :> Post '[JSON] NoContent
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
  } deriving (Show, Eq, Generic)

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
