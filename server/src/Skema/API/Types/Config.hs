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
  , NotificationProviderResponse(..)
  , PushoverProviderResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2, withText)
import qualified Data.Aeson
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

-- | Pushover notification provider for API responses.
data PushoverProviderResponse = PushoverProviderResponse
  { pushoverResponseUserKey :: Text
  , pushoverResponseDevice :: Maybe Text
  , pushoverResponsePriority :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON PushoverProviderResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

instance FromJSON PushoverProviderResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

-- | Notification provider for API responses.
data NotificationProviderResponse
  = NPushover PushoverProviderResponse
  deriving (Show, Eq, Generic)

instance ToJSON NotificationProviderResponse where
  toJSON (NPushover config) = Data.Aeson.object
    [ "type" Data.Aeson..= ("pushover" :: Text)
    , "user_key" Data.Aeson..= pushoverResponseUserKey config
    , "device" Data.Aeson..= pushoverResponseDevice config
    , "priority" Data.Aeson..= pushoverResponsePriority config
    ]

instance FromJSON NotificationProviderResponse where
  parseJSON = Data.Aeson.withObject "NotificationProviderResponse" $ \o -> do
    providerType <- o Data.Aeson..: "type"
    case providerType :: Text of
      "pushover" -> do
        userKey <- o Data.Aeson..: "user_key"
        device <- o Data.Aeson..:? "device"
        priority <- o Data.Aeson..:? "priority" Data.Aeson..!= 0
        pure $ NPushover $ PushoverProviderResponse userKey device priority
      _ -> fail $ "Unknown notification provider type: " <> toString providerType

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
    -- Notification settings
  , configNotificationEnabled :: Bool
  , configNotificationProviders :: [NotificationProviderResponse]
  , configNotificationOnAlbumFound :: Bool
  , configNotificationOnAlbumImported :: Bool
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
    -- Notification settings
  , updateNotificationEnabled :: Maybe Bool
  , updateNotificationProviders :: Maybe [NotificationProviderResponse]
  , updateNotificationOnAlbumFound :: Maybe Bool
  , updateNotificationOnAlbumImported :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ConfigUpdate where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance FromJSON ConfigUpdate where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }
