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
  , DownloadConfig (..)
  , DownloadClient (..)
  , DownloadClientType (..)
  , IndexerConfig (..)
  , Indexer (..)
  , MusicBrainzConfig (..)
  , MusicBrainzServer (..)
  , MediaConfig (..)
  , NotificationConfig (..)
  , NotificationProvider (..)
  , PushoverConfig (..)
    -- * Defaults
  , currentConfigVersion
  , defaultConfig
  , defaultLibraryConfig
  , defaultSystemConfig
  , defaultServerConfig
  , defaultDownloadConfig
  , defaultIndexerConfig
  , defaultMusicBrainzConfig
  , defaultMediaConfig
  , defaultNotificationConfig
    -- * Helpers
  , getMusicBrainzServerUrl
  , hashPassword
  , verifyPassword
  , isHashedPassword
  , downloadClientTypeName
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?), (.!=), object, (.=), Value(..))
import qualified Data.Aeson.KeyMap as KM
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
    pure $ Config version lib sys srv dl idx mb med notif

instance ToJSON Config where
  toJSON (Config version lib sys srv dl idx mb med notif) = object
    [ "version" .= version
    , "library" .= lib
    , "system" .= sys
    , "server" .= srv
    , "download" .= dl
    , "indexers" .= idx
    , "musicbrainz" .= mb
    , "media" .= med
    , "notifications" .= notif
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
  , libraryAutoUpgradeExistingAlbums :: Bool
    -- ^ When discovering albums already in library, automatically set their quality profile to default (enable monitoring/upgrading)
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
    autoUpgradeExisting <- o .:? "auto_upgrade_existing_albums" .!= False
    -- Convert String to OsPath using unsafePerformIO
    -- This is safe because we're converting a constant string from config
    -- Also expand tilde and environment variables in the path
    let osPath = case maybePath of
          Nothing -> Nothing
          Just (pathStr :: String) -> Just $ unsafePerformIO $ do
            expanded <- PathExpansion.expandPathIO (toText pathStr)
            OP.encodeFS (toString expanded)
    pure $ LibraryConfig osPath watch autoScan interval scanOnStartup normalizeFeaturing normalizeFeaturingTo pathFormat fileFormat autoUpgradeExisting

instance ToJSON LibraryConfig where
  toJSON (LibraryConfig maybePath watch autoScan interval scanOnStartup normalizeFeaturing normalizeFeaturingTo pathFormat fileFormat autoUpgradeExisting) =
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
      , "auto_upgrade_existing_albums" .= autoUpgradeExisting
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
  deriving (Show, Eq, Generic)

instance FromJSON DownloadClientType where
  parseJSON = withObject "DownloadClientType" $ \o -> do
    t <- o .: "type"
    case t of
      "sabnzbd" -> pure SABnzbd
      "nzbget" -> pure NZBGet
      "transmission" -> pure Transmission
      "qbittorrent" -> pure QBittorrent
      _ -> fail $ "Unknown download client type: " <> t

instance ToJSON DownloadClientType where
  toJSON SABnzbd = object ["type" .= ("sabnzbd" :: Text)]
  toJSON NZBGet = object ["type" .= ("nzbget" :: Text)]
  toJSON Transmission = object ["type" .= ("transmission" :: Text)]
  toJSON QBittorrent = object ["type" .= ("qbittorrent" :: Text)]

-- | Get the display name for a download client type.
downloadClientTypeName :: DownloadClientType -> Text
downloadClientTypeName SABnzbd = "SABnzbd"
downloadClientTypeName NZBGet = "NZBGet"
downloadClientTypeName Transmission = "Transmission"
downloadClientTypeName QBittorrent = "qBittorrent"

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
    , "url" .= url
    , "api_key" .= apiKey
    , "username" .= username
    , "password" .= password
    , "enabled" .= enabled
    , "download_dir" .= downloadDir
    , "category" .= category
    ]

-- | Download configuration.
data DownloadConfig = DownloadConfig
  { downloadNzbClient :: Maybe DownloadClient
    -- ^ NZB download client (SABnzbd or NZBGet)
  , downloadTorrentClient :: Maybe DownloadClient
    -- ^ Torrent download client (Transmission or qBittorrent)
  , downloadDirectory :: Text
    -- ^ Directory for completed downloads (before import)
  , downloadCheckInterval :: Int
    -- ^ How often to check for completed downloads (in seconds)
  , downloadAutoImport :: Bool
    -- ^ Automatically import completed downloads
  , downloadMinSeeders :: Maybe Int
    -- ^ Minimum seeders for torrents (Nothing = no minimum)
  , downloadMaxSize :: Maybe Int
    -- ^ Maximum download size in MB (Nothing = no limit)
  , downloadReplaceLibraryFiles :: Bool
    -- ^ Delete old library files when upgrading to better quality
  , downloadUseTrash :: Bool
    -- ^ Move deleted files to trash instead of permanently deleting them
  , downloadTrashRetentionDays :: Int
    -- ^ Number of days to keep files in trash before permanent deletion (default: 30)
  } deriving (Show, Eq, Generic)

instance FromJSON DownloadConfig where
  parseJSON = withObject "DownloadConfig" $ \o -> do
    nzbClient <- o .:? "nzb_client"
    torrentClient <- o .:? "torrent_client"
    dir <- o .:? "directory" .!= "~/Downloads/skema"
    checkInterval <- o .:? "check_interval" .!= 60
    autoImport <- o .:? "auto_import" .!= True
    minSeeders <- o .:? "min_seeders"
    maxSize <- o .:? "max_size_mb"
    replaceFiles <- o .:? "replace_library_files" .!= False
    useTrash <- o .:? "use_trash" .!= True
    trashRetentionDays <- o .:? "trash_retention_days" .!= 30
    -- Expand tilde and env vars in directory
    let expandedDir = unsafePerformIO $ PathExpansion.expandPathIO dir
    pure $ DownloadConfig nzbClient torrentClient expandedDir checkInterval autoImport minSeeders maxSize replaceFiles useTrash trashRetentionDays

instance ToJSON DownloadConfig where
  toJSON (DownloadConfig nzbClient torrentClient dir checkInterval autoImport minSeeders maxSize replaceFiles useTrash trashRetentionDays) = object
    [ "nzb_client" .= nzbClient
    , "torrent_client" .= torrentClient
    , "directory" .= dir
    , "check_interval" .= checkInterval
    , "auto_import" .= autoImport
    , "min_seeders" .= minSeeders
    , "max_size_mb" .= maxSize
    , "replace_library_files" .= replaceFiles
    , "use_trash" .= useTrash
    , "trash_retention_days" .= trashRetentionDays
    ]

-- | MusicBrainz server selection.
data MusicBrainzServer
  = OfficialMusicBrainz    -- ^ Official MusicBrainz (musicbrainz.org, 1 req/sec, no auth)
  | HeadphonesVIP          -- ^ Headphones VIP mirror (musicbrainz.codeshy.com, 3 req/sec, requires auth)
  | CustomServer Text      -- ^ Custom MusicBrainz server (provide full URL)
  deriving (Show, Eq, Generic)

instance FromJSON MusicBrainzServer where
  parseJSON = withObject "MusicBrainzServer" $ \o -> do
    serverType <- o .: "server"
    case serverType of
      "official" -> pure OfficialMusicBrainz
      "headphones_vip" -> pure HeadphonesVIP
      "custom" -> do
        url <- o .: "custom_url"
        pure $ CustomServer url
      _ -> fail $ "Unknown MusicBrainz server: " <> serverType

instance ToJSON MusicBrainzServer where
  toJSON OfficialMusicBrainz = object ["server" .= ("official" :: Text)]
  toJSON HeadphonesVIP = object ["server" .= ("headphones_vip" :: Text)]
  toJSON (CustomServer url) = object ["server" .= ("custom" :: Text), "custom_url" .= url]

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
    -- Use MusicBrainzServer's FromJSON instance to properly handle all server types
    serverVal <- parseJSON (Object o)
    username <- o .:? "username"
    password <- o .:? "password"
    albumTypes <- o .:? "album_types" .!= ["Album"]
    excludeSecondaryTypes <- o .:? "exclude_secondary_types" .!= ["Live", "Compilation"]
    pure $ MusicBrainzConfig serverVal username password albumTypes excludeSecondaryTypes

instance ToJSON MusicBrainzConfig where
  toJSON (MusicBrainzConfig serverVal username password albumTypes excludeSecondaryTypes) =
    -- Use MusicBrainzServer's ToJSON instance to properly serialize all server types
    case toJSON serverVal of
      Object serverObj -> Object $ serverObj
        & KM.insert "username" (toJSON username)
        & KM.insert "password" (toJSON password)
        & KM.insert "album_types" (toJSON albumTypes)
        & KM.insert "exclude_secondary_types" (toJSON excludeSecondaryTypes)
      _ -> object  -- Fallback (shouldn't happen)
        [ "username" .= username
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
  , indexerNormalizeQuery :: Bool
    -- ^ Whether to normalize search queries (remove special characters). Default: False
    -- ^ Some older indexers don't handle special characters well (e.g., "AC/DC" -> "ACDC")
    -- ^ Modern indexers like Bullet handle special characters fine, so this is opt-in per indexer
  , indexerResponseFormat :: Text
    -- ^ Response format: "xml", "json", or "auto". Default: "auto"
    -- ^ "auto" accepts whatever the indexer sends and auto-detects the format
    -- ^ Most indexers (including Jackett) only support XML. Modern indexers like Bullet support JSON.
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
    normalizeQuery <- o .:? "normalize_query" .!= False
    responseFormat <- o .:? "response_format" .!= "auto"
    pure $ Indexer name url apiKey username password enabled priority categories normalizeQuery responseFormat

instance ToJSON Indexer where
  toJSON (Indexer name url apiKey username password enabled priority categories normalizeQuery responseFormat) = object
    [ "name" .= name
    , "url" .= url
    , "api_key" .= apiKey
    , "username" .= username
    , "password" .= password
    , "enabled" .= enabled
    , "priority" .= priority
    , "categories" .= categories
    , "normalize_query" .= normalizeQuery
    , "response_format" .= responseFormat
    ]

-- | Indexer configuration.
data IndexerConfig = IndexerConfig
  { indexerList :: [Indexer]
    -- ^ Configured indexers
  , indexerSearchTimeout :: Int
    -- ^ Search timeout per indexer in seconds
  } deriving (Show, Eq, Generic)

instance FromJSON IndexerConfig where
  parseJSON = withObject "IndexerConfig" $ \o -> do
    idxList <- o .:? "list" .!= []
    timeout <- o .:? "search_timeout" .!= 30
    pure $ IndexerConfig idxList timeout

instance ToJSON IndexerConfig where
  toJSON (IndexerConfig idxList timeout) = object
    [ "list" .= idxList
    , "search_timeout" .= timeout
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
  CustomServer url -> url

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
  , libraryAutoUpgradeExistingAlbums = False
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
  , downloadDirectory = "~/Downloads/skema"
  , downloadCheckInterval = 5  -- Check every 5 seconds
  , downloadAutoImport = True
  , downloadMinSeeders = Just 1
  , downloadMaxSize = Nothing
  , downloadReplaceLibraryFiles = False
  , downloadUseTrash = True
  , downloadTrashRetentionDays = 7
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
          , indexerNormalizeQuery = False  -- Bullet handles special characters well
          , indexerResponseFormat = "auto"  -- Auto-detect format (Bullet supports JSON)
          }
      ]
  , indexerSearchTimeout = 30
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
instance Mergeable Config
