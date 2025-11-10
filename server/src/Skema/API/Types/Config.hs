{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Configuration API types.
module Skema.API.Types.Config
  ( ConfigAPI
  , ConfigResponse(..)
  , ConfigUpdate(..)
  , DownloadClientType(..)
  , DownloadClientResponse(..)
  , IndexerResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2, withText)
import GHC.Generics ()
import Servant

-- | Configuration endpoints.
type ConfigAPI = "config" :> Header "Authorization" Text :>
  ( Get '[JSON] ConfigResponse
  :<|> ReqBody '[JSON] ConfigUpdate :> Put '[JSON] ConfigResponse
  )

-- | Download client type for API responses.
data DownloadClientType
  = DCTypeSABnzbd
  | DCTypeNZBGet
  | DCTypeTransmission
  | DCTypeQBittorrent
  deriving (Show, Eq, Generic)

instance ToJSON DownloadClientType where
  toJSON DCTypeSABnzbd = "sabnzbd"
  toJSON DCTypeNZBGet = "nzbget"
  toJSON DCTypeTransmission = "transmission"
  toJSON DCTypeQBittorrent = "qbittorrent"

instance FromJSON DownloadClientType where
  parseJSON = withText "DownloadClientType" $ \t -> case t of
    "sabnzbd" -> pure DCTypeSABnzbd
    "nzbget" -> pure DCTypeNZBGet
    "transmission" -> pure DCTypeTransmission
    "qbittorrent" -> pure DCTypeQBittorrent
    _ -> fail $ "Unknown download client type: " <> toString t

-- | Download client for API responses.
data DownloadClientResponse = DownloadClientResponse
  { dcResponseType :: DownloadClientType
  , dcResponseUrl :: Text
  , dcResponseApiKey :: Maybe Text
  , dcResponseUsername :: Maybe Text
  , dcResponsePassword :: Maybe Text
  , dcResponseEnabled :: Bool
  , dcResponseDownloadDir :: Maybe Text
    -- ^ Download directory (for SABnzbd - where completed downloads are stored)
  , dcResponseCategory :: Maybe Text
    -- ^ Category (for SABnzbd - e.g., "music" or "headphones")
  } deriving (Show, Eq, Generic)

instance ToJSON DownloadClientResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance FromJSON DownloadClientResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

-- | Indexer for API responses.
data IndexerResponse = IndexerResponse
  { indexerResponseName :: Text
  , indexerResponseUrl :: Text
  , indexerResponseApiKey :: Maybe Text
  , indexerResponseUsername :: Maybe Text
  , indexerResponsePassword :: Maybe Text
  , indexerResponseEnabled :: Bool
  , indexerResponsePriority :: Int
  , indexerResponseCategories :: [Int]
  } deriving (Show, Eq, Generic)

instance ToJSON IndexerResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON IndexerResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

-- | Config response.
data ConfigResponse = ConfigResponse
  { -- Library settings
    configLibraryPath :: Maybe Text
  , configLibraryWatch :: Bool
  , configLibraryAutoScan :: Bool
  , configLibraryAutoScanIntervalMins :: Int
  , configLibraryAutoScanOnStartup :: Bool
  , configLibraryNormalizeFeaturing :: Bool
  , configLibraryNormalizeFeaturingTo :: Text
  , configLibraryPathFormat :: Text
  , configLibraryFileFormat :: Text
    -- System settings
  , configSystemWatchConfigFile :: Bool
  , configSystemDatabaseBackend :: Text
  , configSystemDatabasePath :: Text
    -- Server settings
  , configServerHost :: Text
  , configServerPort :: Int
  , configServerUsername :: Maybe Text
  , configServerJwtExpirationHours :: Int
  , configServerAuthEnabled :: Bool  -- Computed field: True if username/password are set
    -- Download settings
  , configDownloadNzbClient :: Maybe DownloadClientResponse
  , configDownloadTorrentClient :: Maybe DownloadClientResponse
  , configDownloadDirectory :: Text
  , configDownloadCheckInterval :: Int
  , configDownloadAutoImport :: Bool
  , configDownloadMinSeeders :: Maybe Int
  , configDownloadMaxSizeMB :: Maybe Int
    -- Indexer settings
  , configIndexersList :: [IndexerResponse]
  , configIndexersSearchTimeout :: Int
    -- MusicBrainz settings
  , configMusicBrainzServer :: Text
  , configMusicBrainzUsername :: Maybe Text
  , configMusicBrainzPassword :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ConfigResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance FromJSON ConfigResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Config update request.
data ConfigUpdate = ConfigUpdate
  { -- Library settings
    updateLibraryPath :: Maybe (Maybe Text)
  , updateLibraryWatch :: Maybe Bool
  , updateLibraryAutoScan :: Maybe Bool
  , updateLibraryAutoScanIntervalMins :: Maybe Int
  , updateLibraryAutoScanOnStartup :: Maybe Bool
  , updateLibraryNormalizeFeaturing :: Maybe Bool
  , updateLibraryNormalizeFeaturingTo :: Maybe Text
  , updateLibraryPathFormat :: Maybe Text
  , updateLibraryFileFormat :: Maybe Text
    -- System settings
  , updateSystemWatchConfigFile :: Maybe Bool
  , updateSystemDatabaseBackend :: Maybe Text
  , updateSystemDatabasePath :: Maybe Text
    -- Server settings
  , updateServerHost :: Maybe Text
  , updateServerPort :: Maybe Int
  , updateServerUsername :: Maybe (Maybe Text)
  , updateServerPassword :: Maybe (Maybe Text)
  , updateServerJwtExpirationHours :: Maybe Int
    -- Download settings
  , updateDownloadNzbClient :: Maybe (Maybe DownloadClientResponse)
  , updateDownloadTorrentClient :: Maybe (Maybe DownloadClientResponse)
  , updateDownloadDirectory :: Maybe Text
  , updateDownloadCheckInterval :: Maybe Int
  , updateDownloadAutoImport :: Maybe Bool
  , updateDownloadMinSeeders :: Maybe (Maybe Int)
  , updateDownloadMaxSizeMB :: Maybe (Maybe Int)
    -- Indexer settings
  , updateIndexersList :: Maybe [IndexerResponse]
  , updateIndexersSearchTimeout :: Maybe Int
    -- MusicBrainz settings
  , updateMusicBrainzServer :: Maybe Text
  , updateMusicBrainzUsername :: Maybe (Maybe Text)
  , updateMusicBrainzPassword :: Maybe (Maybe Text)
  } deriving (Show, Eq, Generic)

instance ToJSON ConfigUpdate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance FromJSON ConfigUpdate where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }
