{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Configuration types for Skema.
--
-- The configuration is loaded from a YAML file and can be updated at runtime.
-- When configuration changes, events are emitted to notify subscribers.
module Skema.Config.Types
  ( -- * Configuration
    Config (..)
  , LibraryConfig (..)
  , SystemConfig (..)
  , ServerConfig (..)
  , PerformanceConfig (..)
  , DownloadConfig (..)
  , DownloadClient (..)
  , DownloadClientType (..)
  , ImportMode (..)
  , DownloadPreference (..)
  , IndexerConfig (..)
  , Indexer (..)
  , ProwlarrConfig (..)
  , MusicBrainzConfig (..)
  , MusicBrainzServer (..)
  , MediaConfig (..)
  , NotificationConfig (..)
  , NotificationProvider (..)
  , PushoverConfig (..)
  , IntegrationsConfig (..)
  , AcoustIdConfig (..)
  , DiscogsConfig (..)
  , SpotifyConfig (..)
  , FanartTvConfig (..)
  , TheAudioDbConfig (..)
    -- * Defaults
  , currentConfigVersion
  , defaultConfig
  , defaultLibraryConfig
  , defaultSystemConfig
  , defaultServerConfig
  , defaultPerformanceConfig
  , defaultDownloadConfig
  , defaultIndexerConfig
  , defaultMusicBrainzConfig
  , defaultMediaConfig
  , defaultNotificationConfig
  , defaultIntegrationsConfig
    -- * Helpers
  , getMusicBrainzServerUrl
  , hashPassword
  , verifyPassword
  , isHashedPassword
  , downloadClientTypeName
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, withText, (.:), (.:?), (.!=), object, (.=), Value(..))
import Skema.Config.Schema (Default(..), Mergeable(..))
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import System.IO.Unsafe (unsafePerformIO)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Text as T
import qualified Skema.Config.PathExpansion as PathExpansion

-- | Top-level configuration.
data Config = Config
  { configVersion :: Int
    -- ^ Config file format version for migrations (current: 1)
  , library :: LibraryConfig
  , system :: SystemConfig
  , server :: ServerConfig
  , download :: DownloadConfig
  , indexers :: IndexerConfig
  , musicbrainz :: MusicBrainzConfig
  , media :: MediaConfig
  , notifications :: NotificationConfig
  , integrations :: IntegrationsConfig
  } deriving (Show, Eq, Generic)

-- | Current config version
currentConfigVersion :: Int
currentConfigVersion = 1

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    -- Default to version 1 if not specified (for backwards compatibility)
    version <- o .:? "version" .!= 1
    lib <- o .:? "library" .!= defaultLibraryConfig
    sys <- o .:? "system" .!= defaultSystemConfig
    srv <- o .:? "server" .!= defaultServerConfig
    dl <- o .:? "download" .!= defaultDownloadConfig
    idx <- o .:? "indexers" .!= defaultIndexerConfig
    mb <- o .:? "musicbrainz" .!= defaultMusicBrainzConfig
    med <- o .:? "media" .!= defaultMediaConfig
    notif <- o .:? "notifications" .!= defaultNotificationConfig
    integ <- o .:? "integrations" .!= defaultIntegrationsConfig
    pure $ Config version lib sys srv dl idx mb med notif integ

instance ToJSON Config where
  toJSON (Config version lib sys srv dl idx mb med notif integ) = object
    [ "version" .= version
    , "library" .= lib
    , "system" .= sys
    , "server" .= srv
    , "download" .= dl
    , "indexers" .= idx
    , "musicbrainz" .= mb
    , "media" .= med
    , "notifications" .= notif
    , "integrations" .= integ
    ]

-- | Library configuration.
data LibraryConfig = LibraryConfig
  { libraryPath :: Maybe OsPath
    -- ^ Path to music library (optional, Nothing means no library configured)
  , libraryWatch :: Bool
    -- ^ Enable file system watching
  , libraryAutoScan :: Bool
    -- ^ Enable automatic periodic scanning
  , libraryAutoScanIntervalMins :: Int
    -- ^ Interval between automatic scans in minutes
  , libraryAutoScanOnStartup :: Bool
    -- ^ Run a full scan on application startup
  , libraryNormalizeFeaturing :: Bool
    -- ^ Normalize "featuring" join phrases in artist credits
  , libraryNormalizeFeaturingTo :: Text
    -- ^ What to normalize "featuring" to (e.g., "feat.")
  , libraryPathFormat :: Text
    -- ^ Template for album directory paths (e.g., "{album_artist}/{year} - {album}/")
  , libraryFileFormat :: Text
    -- ^ Template for track filenames (e.g., "{if:multidisc|{disc:02}-}{track:02} - {title}.{ext}")
  } deriving (Show, Eq, Generic)

instance FromJSON LibraryConfig where
  parseJSON = withObject "LibraryConfig" $ \o -> do
    maybePath <- o .:? "path"
    watch <- o .:? "watch" .!= True
    autoScan <- o .:? "auto_scan" .!= True
    interval <- o .:? "auto_scan_interval_mins" .!= 60
    scanOnStartup <- o .:? "auto_scan_on_startup" .!= True
    normalizeFeaturing <- o .:? "normalize_featuring" .!= False
    normalizeFeaturingTo <- o .:? "normalize_featuring_to" .!= "feat."
    pathFormat <- o .:? "path_format" .!= "{album_artist}/{album} [{year}]"
    fileFormat <- o .:? "file_format" .!= "{track:02} {artist} - {album} {year} - {title}.{ext}"
    -- Convert String to OsPath using unsafePerformIO
    -- This is safe because we're converting a constant string from config
    -- Also expand tilde and environment variables in the path
    let osPath = case maybePath of
          Nothing -> Nothing
          Just (pathStr :: String) -> Just $ unsafePerformIO $ do
            expanded <- PathExpansion.expandPathIO (toText pathStr)
            OP.encodeUtf (toString expanded)
    pure $ LibraryConfig osPath watch autoScan interval scanOnStartup normalizeFeaturing normalizeFeaturingTo pathFormat fileFormat

instance ToJSON LibraryConfig where
  toJSON (LibraryConfig maybePath watch autoScan interval scanOnStartup normalizeFeaturing normalizeFeaturingTo pathFormat fileFormat) =
    -- Convert OsPath back to String for serialization
    let pathField = case maybePath of
          Nothing -> []
          Just path -> ["path" .= (unsafePerformIO $ OP.decodeUtf path :: String)]
    in object $ pathField <>
      [ "watch" .= watch
      , "auto_scan" .= autoScan
      , "auto_scan_interval_mins" .= interval
      , "auto_scan_on_startup" .= scanOnStartup
      , "normalize_featuring" .= normalizeFeaturing
      , "normalize_featuring_to" .= normalizeFeaturingTo
      , "path_format" .= pathFormat
      , "file_format" .= fileFormat
      ]

-- | System configuration.
data SystemConfig = SystemConfig
  { systemWatchConfigFile :: Bool
    -- ^ Watch config file for changes and reload automatically
  , systemDatabasePath :: Text
    -- ^ SQLite database file path
  , systemDataDir :: Maybe Text
    -- ^ Data directory override (Nothing = use platform default)
  , systemCacheDir :: Maybe Text
    -- ^ Cache directory override (Nothing = use platform default)
  } deriving (Show, Eq, Generic)

instance FromJSON SystemConfig where
  parseJSON = withObject "SystemConfig" $ \o -> do
    watchConfig <- o .:? "watch_config_file" .!= True
    dbPath <- o .:? "database_path" .!= "skema.db"
    -- Expand tilde and environment variables in database path
    -- Using unsafePerformIO here is safe because we're just expanding a path string
    let expandedDbPath = unsafePerformIO $ PathExpansion.expandPathIO dbPath

    -- Parse optional directory overrides (with path expansion)
    dataDir <- o .:? "data_dir"
    let expandedDataDir = fmap (unsafePerformIO . PathExpansion.expandPathIO) dataDir

    cacheDir <- o .:? "cache_dir"
    let expandedCacheDir = fmap (unsafePerformIO . PathExpansion.expandPathIO) cacheDir

    pure $ SystemConfig watchConfig expandedDbPath expandedDataDir expandedCacheDir

instance ToJSON SystemConfig where
  toJSON (SystemConfig watchConfig dbPath dataDir cacheDir) = object
    [ "watch_config_file" .= watchConfig
    , "database_path" .= dbPath
    , "data_dir" .= dataDir
    , "cache_dir" .= cacheDir
    ]

-- | Server configuration.
data ServerConfig = ServerConfig
  { serverHost :: Text
    -- ^ Server host to bind to
  , serverPort :: Int
    -- ^ HTTP API server port
  , serverUsername :: Maybe Text
    -- ^ Username for API authentication (can be overridden by SKEMA_USERNAME env var)
  , serverPassword :: Maybe Text
    -- ^ Password for API authentication (can be overridden by SKEMA_PASSWORD env var)
  , serverJwtSecret :: Maybe Text
    -- ^ JWT signing secret (auto-generated if not provided, persisted to config)
  , serverJwtExpirationHours :: Int
    -- ^ JWT token expiration time in hours (default: 24)
  } deriving (Show, Eq, Generic)

instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o -> do
    host <- o .:? "host" .!= "127.0.0.1"
    port <- o .:? "port" .!= 8182
    username <- o .:? "username"
    password <- o .:? "password"
    jwtSecret <- o .:? "jwt_secret"
    jwtExpHours <- o .:? "jwt_expiration_hours" .!= 24
    pure $ ServerConfig host port username password jwtSecret jwtExpHours

instance ToJSON ServerConfig where
  toJSON (ServerConfig host port username password jwtSecret jwtExpHours) = object
    [ "host" .= host
    , "port" .= port
    , "username" .= username
    , "password" .= password
    -- Note: password is now included since it's bcrypt hashed
    , "jwt_secret" .= jwtSecret
    , "jwt_expiration_hours" .= jwtExpHours
    ]

-- | Download client type.
data DownloadClientType
  = SABnzbd
  | NZBGet
  | Transmission
  | QBittorrent
  | Deluge
  deriving (Show, Eq, Generic)

instance FromJSON DownloadClientType where
  parseJSON = withObject "DownloadClientType" $ \o -> do
    t <- o .: "type"
    case t of
      "sabnzbd" -> pure SABnzbd
      "nzbget" -> pure NZBGet
      "transmission" -> pure Transmission
      "qbittorrent" -> pure QBittorrent
      "deluge" -> pure Deluge
      _ -> fail $ "Unknown download client type: " <> t

instance ToJSON DownloadClientType where
  toJSON SABnzbd = object ["type" .= ("sabnzbd" :: Text)]
  toJSON NZBGet = object ["type" .= ("nzbget" :: Text)]
  toJSON Transmission = object ["type" .= ("transmission" :: Text)]
  toJSON QBittorrent = object ["type" .= ("qbittorrent" :: Text)]
  toJSON Deluge = object ["type" .= ("deluge" :: Text)]

-- | Get the display name for a download client type.
downloadClientTypeName :: DownloadClientType -> Text
downloadClientTypeName SABnzbd = "SABnzbd"
downloadClientTypeName NZBGet = "NZBGet"
downloadClientTypeName Transmission = "Transmission"
downloadClientTypeName QBittorrent = "qBittorrent"
downloadClientTypeName Deluge = "Deluge"

-- | Download client configuration.
data DownloadClient = DownloadClient
  { dcType :: DownloadClientType
    -- ^ Client type
  , dcUrl :: Text
    -- ^ Base URL (e.g., "http://localhost:8080")
  , dcApiKey :: Maybe Text
    -- ^ API key (for SABnzbd, NZBGet)
  , dcUsername :: Maybe Text
    -- ^ Username (for Transmission, qBittorrent)
  , dcPassword :: Maybe Text
    -- ^ Password (for Transmission, qBittorrent)
  , dcEnabled :: Bool
    -- ^ Whether this client is enabled
  , dcDownloadDir :: Maybe Text
    -- ^ Download directory (for SABnzbd - where completed downloads are stored)
  , dcCategory :: Maybe Text
    -- ^ Category (for SABnzbd - e.g., "music" or "headphones")
  } deriving (Show, Eq, Generic)

instance FromJSON DownloadClient where
  parseJSON = withObject "DownloadClient" $ \o -> do
    typeStr <- o .: "type"
    clientType <- case typeStr of
      "sabnzbd" -> pure SABnzbd
      "nzbget" -> pure NZBGet
      "transmission" -> pure Transmission
      "qbittorrent" -> pure QBittorrent
      "deluge" -> pure Deluge
      _ -> fail $ "Unknown download client type: " <> typeStr
    url <- o .: "url"
    apiKey <- o .:? "api_key"
    username <- o .:? "username"
    password <- o .:? "password"
    enabled <- o .:? "enabled" .!= True
    downloadDir <- o .:? "download_dir"
    category <- o .:? "category"
    pure $ DownloadClient clientType url apiKey username password enabled downloadDir category

instance ToJSON DownloadClient where
  toJSON (DownloadClient clientType url apiKey username password enabled downloadDir category) = object
    [ "type" .= case clientType of
        SABnzbd -> "sabnzbd" :: Text
        NZBGet -> "nzbget"
        Transmission -> "transmission"
        QBittorrent -> "qbittorrent"
        Deluge -> "deluge"
    , "url" .= url
    , "api_key" .= apiKey
    , "username" .= username
    , "password" .= password
    , "enabled" .= enabled
    , "download_dir" .= downloadDir
    , "category" .= category
    ]

-- | Import mode for moving files from download directory to library.
data ImportMode
  = ImportMove
    -- ^ Move files to library, delete originals after successful move
  | ImportCopy
    -- ^ Copy files to library, keep originals (manual cleanup required)
  | ImportHardlink
    -- ^ Create hardlinks in library (same filesystem required, saves space)
  | ImportSymlink
    -- ^ Create symlinks in library (files stay in download directory)
  deriving (Show, Eq, Generic)

instance FromJSON ImportMode where
  parseJSON = withText "ImportMode" $ \t -> case T.toLower t of
    "move" -> pure ImportMove
    "copy" -> pure ImportCopy
    "hardlink" -> pure ImportHardlink
    "symlink" -> pure ImportSymlink
    _ -> fail $ "Unknown import mode: " <> T.unpack t <> ". Valid options: move, copy, hardlink, symlink"

instance ToJSON ImportMode where
  toJSON ImportMove = "move"
  toJSON ImportCopy = "copy"
  toJSON ImportHardlink = "hardlink"
  toJSON ImportSymlink = "symlink"

-- | Preferred download type when both NZB and torrent are available.
data DownloadPreference
  = PreferNzb
    -- ^ Prefer NZB downloads (Usenet) when available
  | PreferTorrent
    -- ^ Prefer torrent downloads when available
  | PreferBest
    -- ^ Automatically choose based on quality/availability
  deriving (Show, Eq, Generic)

instance FromJSON DownloadPreference where
  parseJSON = withText "DownloadPreference" $ \t -> case T.toLower t of
    "nzb" -> pure PreferNzb
    "usenet" -> pure PreferNzb
    "torrent" -> pure PreferTorrent
    "best" -> pure PreferBest
    "auto" -> pure PreferBest
    _ -> fail $ "Unknown download preference: " <> T.unpack t <> ". Valid options: nzb, torrent, best"

instance ToJSON DownloadPreference where
  toJSON PreferNzb = "nzb"
  toJSON PreferTorrent = "torrent"
  toJSON PreferBest = "best"

-- | Download configuration.
data DownloadConfig = DownloadConfig
  { downloadNzbClient :: Maybe DownloadClient
    -- ^ NZB download client (SABnzbd or NZBGet)
  , downloadTorrentClient :: Maybe DownloadClient
    -- ^ Torrent download client (Transmission or qBittorrent)
  , downloadPreference :: DownloadPreference
    -- ^ Preferred download type when both are available
  , downloadDirectory :: Text
    -- ^ Directory for completed downloads (before import)
  , downloadCheckInterval :: Int
    -- ^ How often to check for completed downloads (in seconds)
  , downloadAutoImport :: Bool
    -- ^ Automatically import completed downloads
  , downloadRefreshArtistOnImport :: Bool
    -- ^ Refresh artist discography when a download is imported
  , downloadImportMode :: ImportMode
    -- ^ How to import files: move, copy, hardlink, or symlink
  , downloadDeleteAfterImport :: Bool
    -- ^ Delete source files after successful import (only for move/copy modes)
  , downloadVerifyBeforeDelete :: Bool
    -- ^ Verify all files copied successfully before deleting source
  , downloadMinSeeders :: Maybe Int
    -- ^ Minimum seeders for torrents (Nothing = no minimum)
  , downloadMaxSize :: Maybe Int
    -- ^ Maximum download size in MB (Nothing = no limit)
  } deriving (Show, Eq, Generic)

instance FromJSON DownloadConfig where
  parseJSON = withObject "DownloadConfig" $ \o -> do
    nzbClient <- o .:? "nzb_client"
    torrentClient <- o .:? "torrent_client"
    preference <- o .:? "preference" .!= PreferBest
    dir <- o .:? "directory" .!= "~/Downloads/skema"
    checkInterval <- o .:? "check_interval" .!= 60
    autoImport <- o .:? "auto_import" .!= True
    refreshArtistOnImport <- o .:? "refresh_artist_on_import" .!= False
    importMode <- o .:? "import_mode" .!= ImportMove
    deleteAfterImport <- o .:? "delete_after_import" .!= True
    verifyBeforeDelete <- o .:? "verify_before_delete" .!= True
    minSeeders <- o .:? "min_seeders"
    maxSize <- o .:? "max_size_mb"
    -- Expand tilde and env vars in directory
    let expandedDir = unsafePerformIO $ PathExpansion.expandPathIO dir
    pure $ DownloadConfig nzbClient torrentClient preference expandedDir checkInterval autoImport refreshArtistOnImport importMode deleteAfterImport verifyBeforeDelete minSeeders maxSize

instance ToJSON DownloadConfig where
  toJSON (DownloadConfig nzbClient torrentClient preference dir checkInterval autoImport refreshArtistOnImport importMode deleteAfterImport verifyBeforeDelete minSeeders maxSize) = object
    [ "nzb_client" .= nzbClient
    , "torrent_client" .= torrentClient
    , "preference" .= preference
    , "directory" .= dir
    , "check_interval" .= checkInterval
    , "auto_import" .= autoImport
    , "refresh_artist_on_import" .= refreshArtistOnImport
    , "import_mode" .= importMode
    , "delete_after_import" .= deleteAfterImport
    , "verify_before_delete" .= verifyBeforeDelete
    , "min_seeders" .= minSeeders
    , "max_size_mb" .= maxSize
    ]

-- | MusicBrainz server selection.
data MusicBrainzServer
  = OfficialMusicBrainz    -- ^ Official MusicBrainz (musicbrainz.org, 1 req/sec, no auth)
  | HeadphonesVIP          -- ^ Headphones VIP mirror (musicbrainz.codeshy.com, 3 req/sec, requires auth)
  deriving (Show, Eq, Generic)

instance FromJSON MusicBrainzServer where
  parseJSON = withObject "MusicBrainzServer" $ \o -> do
    serverType <- o .: "server"
    case serverType of
      "official" -> pure OfficialMusicBrainz
      "headphones_vip" -> pure HeadphonesVIP
      _ -> fail $ "Unknown MusicBrainz server: " <> serverType

instance ToJSON MusicBrainzServer where
  toJSON OfficialMusicBrainz = object ["server" .= ("official" :: Text)]
  toJSON HeadphonesVIP = object ["server" .= ("headphones_vip" :: Text)]

-- | MusicBrainz configuration.
data MusicBrainzConfig = MusicBrainzConfig
  { mbServer :: MusicBrainzServer
    -- ^ Which MusicBrainz server to use
  , mbUsername :: Maybe Text
    -- ^ Username for Headphones VIP (required if using VIP)
  , mbPassword :: Maybe Text
    -- ^ Password for Headphones VIP (required if using VIP)
  , mbAlbumTypes :: [Text]
    -- ^ Primary album types to fetch (e.g., ["Album", "EP"])
    -- Default: ["Album"] (studio albums only)
  , mbExcludeSecondaryTypes :: [Text]
    -- ^ Secondary types to exclude (e.g., ["Live", "Compilation"])
    -- Default: ["Live", "Compilation"] (no live or compilation albums)
  } deriving (Show, Eq, Generic)

instance FromJSON MusicBrainzConfig where
  parseJSON = withObject "MusicBrainzConfig" $ \o -> do
    serverStr <- o .:? "server" .!= "official"
    serverVal <- case serverStr of
      "official" -> pure OfficialMusicBrainz
      "headphones_vip" -> pure HeadphonesVIP
      _ -> fail $ "Unknown MusicBrainz server: " <> serverStr
    username <- o .:? "username"
    password <- o .:? "password"
    albumTypes <- o .:? "album_types" .!= ["Album"]
    excludeSecondaryTypes <- o .:? "exclude_secondary_types" .!= ["Live", "Compilation"]
    pure $ MusicBrainzConfig serverVal username password albumTypes excludeSecondaryTypes

instance ToJSON MusicBrainzConfig where
  toJSON (MusicBrainzConfig serverVal username password albumTypes excludeSecondaryTypes) = object
    [ "server" .= case serverVal of
        OfficialMusicBrainz -> "official" :: Text
        HeadphonesVIP -> "headphones_vip"
    , "username" .= username
    , "password" .= password
    , "album_types" .= albumTypes
    , "exclude_secondary_types" .= excludeSecondaryTypes
    ]

-- | Indexer configuration (Newznab/Torznab).
data Indexer = Indexer
  { indexerName :: Text
    -- ^ Display name
  , indexerUrl :: Text
    -- ^ Base URL (e.g., "https://bullet.codeshy.com")
  , indexerApiKey :: Maybe Text
    -- ^ API key (if required)
  , indexerUsername :: Maybe Text
    -- ^ HTTP basic auth username (e.g., for Bullet)
  , indexerPassword :: Maybe Text
    -- ^ HTTP basic auth password
  , indexerEnabled :: Bool
    -- ^ Whether this indexer is enabled
  , indexerPriority :: Int
    -- ^ Search priority (higher = searched first)
  , indexerCategories :: [Int]
    -- ^ Newznab category IDs (e.g., [3000] for Audio, [3010] for MP3)
  } deriving (Show, Eq, Generic)

instance FromJSON Indexer where
  parseJSON = withObject "Indexer" $ \o -> do
    name <- o .: "name"
    url <- o .: "url"
    apiKey <- o .:? "api_key"
    username <- o .:? "username"
    password <- o .:? "password"
    enabled <- o .:? "enabled" .!= True
    priority <- o .:? "priority" .!= 0
    categories <- o .:? "categories" .!= [3000, 3010] -- Audio, MP3
    pure $ Indexer name url apiKey username password enabled priority categories

instance ToJSON Indexer where
  toJSON (Indexer name url apiKey username password enabled priority categories) = object
    [ "name" .= name
    , "url" .= url
    , "api_key" .= apiKey
    , "username" .= username
    , "password" .= password
    , "enabled" .= enabled
    , "priority" .= priority
    , "categories" .= categories
    ]

-- | Prowlarr configuration for indexer aggregation.
data ProwlarrConfig = ProwlarrConfig
  { prowlarrUrl :: Text
    -- ^ Prowlarr base URL (e.g., "http://localhost:9696")
  , prowlarrApiKey :: Text
    -- ^ Prowlarr API key
  , prowlarrEnabled :: Bool
    -- ^ Whether Prowlarr integration is enabled
  , prowlarrSyncIndexers :: Bool
    -- ^ Automatically sync indexers from Prowlarr
  } deriving (Show, Eq, Generic)

instance FromJSON ProwlarrConfig where
  parseJSON = withObject "ProwlarrConfig" $ \o -> do
    url <- o .: "url"
    apiKey <- o .: "api_key"
    enabled <- o .:? "enabled" .!= True
    syncIndexers <- o .:? "sync_indexers" .!= True
    pure $ ProwlarrConfig url apiKey enabled syncIndexers

instance ToJSON ProwlarrConfig where
  toJSON (ProwlarrConfig url apiKey enabled syncIndexers) = object
    [ "url" .= url
    , "api_key" .= apiKey
    , "enabled" .= enabled
    , "sync_indexers" .= syncIndexers
    ]

-- | Indexer configuration.
data IndexerConfig = IndexerConfig
  { indexerList :: [Indexer]
    -- ^ Configured indexers (manual or synced from Prowlarr)
  , indexerSearchTimeout :: Int
    -- ^ Search timeout per indexer in seconds
  , indexerProwlarr :: Maybe ProwlarrConfig
    -- ^ Optional Prowlarr integration for indexer aggregation
  } deriving (Show, Eq, Generic)

instance FromJSON IndexerConfig where
  parseJSON = withObject "IndexerConfig" $ \o -> do
    idxList <- o .:? "list" .!= []
    timeout <- o .:? "search_timeout" .!= 30
    prowlarr <- o .:? "prowlarr"
    pure $ IndexerConfig idxList timeout prowlarr

instance ToJSON IndexerConfig where
  toJSON (IndexerConfig idxList timeout prowlarr) = object
    [ "list" .= idxList
    , "search_timeout" .= timeout
    , "prowlarr" .= prowlarr
    ]


-- | Check if a password is already hashed with bcrypt.
--
-- Bcrypt hashes start with $2a$, $2b$, or $2y$
isHashedPassword :: Text -> Bool
isHashedPassword password =
  "$2a$" `T.isPrefixOf` password
    || "$2b$" `T.isPrefixOf` password
    || "$2y$" `T.isPrefixOf` password

-- | Hash a password using bcrypt with default cost (10).
--
-- Returns Nothing if hashing fails.
hashPassword :: Text -> IO (Maybe Text)
hashPassword plaintext = do
  let passBytes = encodeUtf8 plaintext
  maybeHash <- BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy passBytes
  pure $ fmap decodeUtf8 maybeHash

-- | Verify a password against a bcrypt hash.
--
-- Returns True if the password matches the hash.
verifyPassword :: Text -> Text -> Bool
verifyPassword plaintext hash =
  let passBytes = encodeUtf8 plaintext
      hashBytes = encodeUtf8 hash
   in BCrypt.validatePassword hashBytes passBytes

-- | Get the MusicBrainz server URL based on configuration.
getMusicBrainzServerUrl :: MusicBrainzConfig -> Text
getMusicBrainzServerUrl cfg = case mbServer cfg of
  OfficialMusicBrainz -> "https://musicbrainz.org"
  HeadphonesVIP -> "https://musicbrainz.codeshy.com"

-- | Media configuration (artist images, album covers).
data MediaConfig = MediaConfig
  { mediaLastFmApiKey :: Maybe Text
    -- ^ Last.fm API key (optional, for fallback)
  } deriving (Show, Eq, Generic)

instance FromJSON MediaConfig where
  parseJSON = withObject "MediaConfig" $ \o -> do
    lastFmKey <- o .:? "lastfm_api_key"
    pure $ MediaConfig lastFmKey

instance ToJSON MediaConfig where
  toJSON (MediaConfig lastFmKey) = object
    [ "lastfm_api_key" .= lastFmKey
    ]

-- | Pushover notification provider configuration.
data PushoverConfig = PushoverConfig
  { pushoverUserKey :: Text
    -- ^ Pushover user/group key
  , pushoverDevice :: Maybe Text
    -- ^ Optional device name to send to (Nothing = all devices)
  , pushoverPriority :: Int
    -- ^ Priority: -2 (lowest) to 2 (emergency), default 0
  } deriving (Show, Eq, Generic)

instance FromJSON PushoverConfig where
  parseJSON = withObject "PushoverConfig" $ \o -> do
    userKey <- o .: "user_key"
    device <- o .:? "device"
    priority <- o .:? "priority" .!= 0
    pure $ PushoverConfig userKey device priority

instance ToJSON PushoverConfig where
  toJSON (PushoverConfig userKey device priority) = object
    [ "user_key" .= userKey
    , "device" .= device
    , "priority" .= priority
    ]

-- | Notification provider configuration.
data NotificationProvider
  = PushoverProvider PushoverConfig
  deriving (Show, Eq, Generic)

instance FromJSON NotificationProvider where
  parseJSON = withObject "NotificationProvider" $ \o -> do
    providerType <- o .: "type"
    case providerType of
      "pushover" -> do
        config <- parseJSON (Object o)
        pure $ PushoverProvider config
      _ -> fail $ "Unknown notification provider: " <> providerType

instance ToJSON NotificationProvider where
  toJSON (PushoverProvider config) =
    case toJSON config of
      Object obj -> Object (obj <> "type" .= ("pushover" :: Text))
      other -> other  -- Shouldn't happen for PushoverConfig

-- | Notification configuration.
data NotificationConfig = NotificationConfig
  { notificationEnabled :: Bool
    -- ^ Enable/disable all notifications
  , notificationProviders :: [NotificationProvider]
    -- ^ List of notification providers
  , notificationOnAlbumFound :: Bool
    -- ^ Send notification when new wanted albums are found
  , notificationOnAlbumDownloaded :: Bool
    -- ^ Send notification when albums are downloaded
  , notificationOnAlbumImported :: Bool
    -- ^ Send notification when albums are imported to library
  } deriving (Show, Eq, Generic)

instance FromJSON NotificationConfig where
  parseJSON = withObject "NotificationConfig" $ \o -> do
    enabled <- o .:? "enabled" .!= False
    providers <- o .:? "providers" .!= []
    onAlbumFound <- o .:? "on_album_found" .!= True
    onAlbumDownloaded <- o .:? "on_album_downloaded" .!= True
    onAlbumImported <- o .:? "on_album_imported" .!= True
    pure $ NotificationConfig enabled providers onAlbumFound onAlbumDownloaded onAlbumImported

instance ToJSON NotificationConfig where
  toJSON (NotificationConfig enabled providers onAlbumFound onAlbumDownloaded onAlbumImported) = object
    [ "enabled" .= enabled
    , "providers" .= providers
    , "on_album_found" .= onAlbumFound
    , "on_album_downloaded" .= onAlbumDownloaded
    , "on_album_imported" .= onAlbumImported
    ]

-- | External integrations configuration.
--
-- These are optional third-party services that can enhance Skema's capabilities:
-- - AcoustID: Audio fingerprinting for identifying unknown tracks
-- - Discogs: Alternative metadata source, especially for rare releases
-- - Spotify: Fuzzy search and popularity data
-- - Fanart.tv: High-quality artwork
-- - TheAudioDB: Artist images and biographies
data IntegrationsConfig = IntegrationsConfig
  { integrationsAcoustId :: Maybe AcoustIdConfig
    -- ^ AcoustID audio fingerprinting (for identifying unknown tracks)
  , integrationsDiscogs :: Maybe DiscogsConfig
    -- ^ Discogs API (alternative metadata source)
  , integrationsSpotify :: Maybe SpotifyConfig
    -- ^ Spotify API (fuzzy search, popularity)
  , integrationsFanartTv :: Maybe FanartTvConfig
    -- ^ Fanart.tv API (high-quality artwork)
  , integrationsTheAudioDb :: Maybe TheAudioDbConfig
    -- ^ TheAudioDB API (artist images, biographies)
  } deriving (Show, Eq, Generic)

instance FromJSON IntegrationsConfig where
  parseJSON = withObject "IntegrationsConfig" $ \o -> do
    acoustId <- o .:? "acoustid"
    discogs <- o .:? "discogs"
    spotify <- o .:? "spotify"
    fanartTv <- o .:? "fanart_tv"
    theAudioDb <- o .:? "theaudiodb"
    pure $ IntegrationsConfig acoustId discogs spotify fanartTv theAudioDb

instance ToJSON IntegrationsConfig where
  toJSON (IntegrationsConfig acoustId discogs spotify fanartTv theAudioDb) = object
    [ "acoustid" .= acoustId
    , "discogs" .= discogs
    , "spotify" .= spotify
    , "fanart_tv" .= fanartTv
    , "theaudiodb" .= theAudioDb
    ]

-- | AcoustID configuration for audio fingerprinting.
data AcoustIdConfig = AcoustIdConfig
  { acoustIdApiKey :: Text
    -- ^ AcoustID API key (get one at https://acoustid.org/api-key)
  , acoustIdEnabled :: Bool
    -- ^ Enable AcoustID integration
  } deriving (Show, Eq, Generic)

instance FromJSON AcoustIdConfig where
  parseJSON = withObject "AcoustIdConfig" $ \o -> do
    apiKey <- o .: "api_key"
    enabled <- o .:? "enabled" .!= True
    pure $ AcoustIdConfig apiKey enabled

instance ToJSON AcoustIdConfig where
  toJSON (AcoustIdConfig apiKey enabled) = object
    [ "api_key" .= apiKey
    , "enabled" .= enabled
    ]

-- | Discogs configuration for alternative metadata.
data DiscogsConfig = DiscogsConfig
  { discogsPersonalAccessToken :: Text
    -- ^ Discogs personal access token (get one at https://www.discogs.com/settings/developers)
  , discogsEnabled :: Bool
    -- ^ Enable Discogs integration
  } deriving (Show, Eq, Generic)

instance FromJSON DiscogsConfig where
  parseJSON = withObject "DiscogsConfig" $ \o -> do
    token <- o .: "personal_access_token"
    enabled <- o .:? "enabled" .!= True
    pure $ DiscogsConfig token enabled

instance ToJSON DiscogsConfig where
  toJSON (DiscogsConfig token enabled) = object
    [ "personal_access_token" .= token
    , "enabled" .= enabled
    ]

-- | Spotify configuration for fuzzy search and popularity data.
data SpotifyConfig = SpotifyConfig
  { spotifyClientId :: Text
    -- ^ Spotify client ID (get one at https://developer.spotify.com/dashboard)
  , spotifyClientSecret :: Text
    -- ^ Spotify client secret
  , spotifyEnabled :: Bool
    -- ^ Enable Spotify integration
  } deriving (Show, Eq, Generic)

instance FromJSON SpotifyConfig where
  parseJSON = withObject "SpotifyConfig" $ \o -> do
    clientId <- o .: "client_id"
    clientSecret <- o .: "client_secret"
    enabled <- o .:? "enabled" .!= True
    pure $ SpotifyConfig clientId clientSecret enabled

instance ToJSON SpotifyConfig where
  toJSON (SpotifyConfig clientId clientSecret enabled) = object
    [ "client_id" .= clientId
    , "client_secret" .= clientSecret
    , "enabled" .= enabled
    ]

-- | Fanart.tv configuration for high-quality artwork.
data FanartTvConfig = FanartTvConfig
  { fanartTvApiKey :: Text
    -- ^ Fanart.tv API key (get one at https://fanart.tv/get-an-api-key/)
  , fanartTvEnabled :: Bool
    -- ^ Enable Fanart.tv integration
  } deriving (Show, Eq, Generic)

instance FromJSON FanartTvConfig where
  parseJSON = withObject "FanartTvConfig" $ \o -> do
    apiKey <- o .: "api_key"
    enabled <- o .:? "enabled" .!= True
    pure $ FanartTvConfig apiKey enabled

instance ToJSON FanartTvConfig where
  toJSON (FanartTvConfig apiKey enabled) = object
    [ "api_key" .= apiKey
    , "enabled" .= enabled
    ]

-- | TheAudioDB configuration for artist images and biographies.
data TheAudioDbConfig = TheAudioDbConfig
  { theAudioDbApiKey :: Text
    -- ^ TheAudioDB API key (get one at https://www.theaudiodb.com/api_guide.php)
  , theAudioDbEnabled :: Bool
    -- ^ Enable TheAudioDB integration
  } deriving (Show, Eq, Generic)

instance FromJSON TheAudioDbConfig where
  parseJSON = withObject "TheAudioDbConfig" $ \o -> do
    apiKey <- o .: "api_key"
    enabled <- o .:? "enabled" .!= True
    pure $ TheAudioDbConfig apiKey enabled

instance ToJSON TheAudioDbConfig where
  toJSON (TheAudioDbConfig apiKey enabled) = object
    [ "api_key" .= apiKey
    , "enabled" .= enabled
    ]

-- ============================================================================
-- Default Configurations
-- ============================================================================

-- | Default library configuration.
defaultLibraryConfig :: LibraryConfig
defaultLibraryConfig = LibraryConfig
  { libraryPath = Nothing
  , libraryWatch = True
  , libraryAutoScan = True
  , libraryAutoScanIntervalMins = 60
  , libraryAutoScanOnStartup = True
  , libraryNormalizeFeaturing = False
  , libraryNormalizeFeaturingTo = "feat."
  , libraryPathFormat = "{album_artist}/{album} [{year}]"
  , libraryFileFormat = "{track:02} {artist} - {album} {year} - {title}.{ext}"
  }

-- | Default system configuration.
defaultSystemConfig :: SystemConfig
defaultSystemConfig = SystemConfig
  { systemWatchConfigFile = True
  , systemDatabasePath = "skema.db"
  , systemDataDir = Nothing      -- Use platform default
  , systemCacheDir = Nothing     -- Use platform default
  }

-- | Default server configuration.
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverHost = "127.0.0.1"
  , serverPort = 8182
  , serverUsername = Nothing
  , serverPassword = Nothing
  , serverJwtSecret = Nothing  -- Auto-generated on first run
  , serverJwtExpirationHours = 24  -- 24 hours default
  }

-- | Default download configuration.
defaultDownloadConfig :: DownloadConfig
defaultDownloadConfig = DownloadConfig
  { downloadNzbClient = Nothing
  , downloadTorrentClient = Nothing
  , downloadPreference = PreferBest  -- Auto-select best available
  , downloadDirectory = "~/Downloads/skema"
  , downloadCheckInterval = 5  -- Check every 5 seconds
  , downloadAutoImport = True
  , downloadRefreshArtistOnImport = False
  , downloadImportMode = ImportMove  -- Move files by default
  , downloadDeleteAfterImport = True  -- Delete source after successful import
  , downloadVerifyBeforeDelete = True  -- Verify files before deleting source
  , downloadMinSeeders = Just 1
  , downloadMaxSize = Nothing
  }

-- | Default indexer configuration with Bullet preconfigured.
defaultIndexerConfig :: IndexerConfig
defaultIndexerConfig = IndexerConfig
  { indexerList =
      [ Indexer
          { indexerName = "Bullet"
          , indexerUrl = "https://bullet.codeshy.com"
          , indexerApiKey = Nothing
          , indexerUsername = Nothing  -- User needs to configure
          , indexerPassword = Nothing  -- User needs to configure
          , indexerEnabled = False     -- Disabled until credentials provided
          , indexerPriority = 10
          , indexerCategories = [3000, 3010] -- Audio, MP3
          }
      ]
  , indexerSearchTimeout = 30
  , indexerProwlarr = Nothing  -- Optional Prowlarr integration
  }

-- | Default MusicBrainz configuration (official server).
defaultMusicBrainzConfig :: MusicBrainzConfig
defaultMusicBrainzConfig = MusicBrainzConfig
  { mbServer = OfficialMusicBrainz
  , mbUsername = Nothing
  , mbPassword = Nothing
  , mbAlbumTypes = ["Album"]  -- Studio albums only by default
  , mbExcludeSecondaryTypes = ["Live", "Compilation"]  -- Exclude live and compilation albums
  }

-- | Default media configuration.
--
-- Uses hardcoded app-level fanart.tv API key.
-- User can optionally configure Last.fm API key for additional fallback.
defaultMediaConfig :: MediaConfig
defaultMediaConfig = MediaConfig
  { mediaLastFmApiKey = Nothing
  }

-- | Default notification configuration.
--
-- Notifications are disabled by default.
-- Users need to configure at least one provider to enable notifications.
defaultNotificationConfig :: NotificationConfig
defaultNotificationConfig = NotificationConfig
  { notificationEnabled = False
  , notificationProviders = []
  , notificationOnAlbumFound = True
  , notificationOnAlbumDownloaded = True
  , notificationOnAlbumImported = True
  }

-- | Default integrations configuration.
--
-- All integrations are disabled by default.
-- Users can opt-in by providing API keys for the services they want to use.
defaultIntegrationsConfig :: IntegrationsConfig
defaultIntegrationsConfig = IntegrationsConfig
  { integrationsAcoustId = Nothing
  , integrationsDiscogs = Nothing
  , integrationsSpotify = Nothing
  , integrationsFanartTv = Nothing
  , integrationsTheAudioDb = Nothing
  }

-- | Default configuration with all defaults.
defaultConfig :: Config
defaultConfig = Config
  { configVersion = currentConfigVersion
  , library = defaultLibraryConfig
  , system = defaultSystemConfig
  , server = defaultServerConfig
  , download = defaultDownloadConfig
  , indexers = defaultIndexerConfig
  , musicbrainz = defaultMusicBrainzConfig
  , media = defaultMediaConfig
  , notifications = defaultNotificationConfig
  , integrations = defaultIntegrationsConfig
  }

-- ============================================================================
-- Default and Mergeable instances (for Schema2 integration)
-- ============================================================================

-- | Default instance for MusicBrainzServer
instance Default MusicBrainzServer where
  def = OfficialMusicBrainz

-- | Config section instances - use Generic derivation
-- These allow the schema system to work with existing types

instance Default LibraryConfig where def = defaultLibraryConfig
instance Default SystemConfig where def = defaultSystemConfig
instance Default ServerConfig where def = defaultServerConfig
instance Default DownloadConfig where def = defaultDownloadConfig
instance Default IndexerConfig where def = defaultIndexerConfig
instance Default MusicBrainzConfig where def = defaultMusicBrainzConfig
instance Default MediaConfig where def = defaultMediaConfig
instance Default NotificationConfig where def = defaultNotificationConfig
instance Default IntegrationsConfig where def = defaultIntegrationsConfig
instance Default Config where def = defaultConfig

-- | Mergeable instances - use Generic derivation
instance Mergeable LibraryConfig
instance Mergeable SystemConfig
instance Mergeable ServerConfig
instance Mergeable DownloadConfig
instance Mergeable IndexerConfig
instance Mergeable MusicBrainzConfig
instance Mergeable MediaConfig
instance Mergeable NotificationConfig
instance Mergeable IntegrationsConfig
instance Mergeable Config
