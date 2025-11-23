{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Simple, pragmatic config schema system.
--
-- Philosophy:
-- - Record types ARE the source of truth for structure
-- - Metadata is attached via a simple list (checked at startup)
-- - Generic provides defaults, merge, JSON
-- - Env vars are explicit per-field
--
-- Usage:
-- @
-- data LibraryConfig = LibraryConfig
--   { libraryPath :: Maybe OsPath
--   , libraryWatch :: Bool
--   , libraryPathFormat :: Text
--   } deriving (Generic, Show, Eq)
--
-- instance Default LibraryConfig  -- Uses Generic
-- instance Mergeable LibraryConfig  -- Uses Generic
--
-- librarySchema :: Schema
-- librarySchema = schema "library" "Library configuration"
--   [ "path"        .:: "Path to music library" & envVar "SKEMA_LIBRARY_PATH"
--   , "watch"       .:: "Watch for file changes" & envVar "SKEMA_LIBRARY_WATCH"
--   , "path_format" .:: "Directory structure format"
--   ]
-- @
module Skema.Config.Schema
  ( -- * Field types
    FieldType(..)
    -- * Field metadata
  , FieldMeta(..)
  , (.::)
  , envVar
  , cliFlag
  , cliShort
  , example
  , sensitive
  , required
  , fieldType
  , pathField
  , boolField
  , intField
  , enumField
    -- * Schema
  , Schema(..)
  , schema
  , schemaToJSON
    -- * Default values (Generic)
  , Default(..)
  , GDefault(..)
    -- * Merging (Generic)
  , Mergeable(..)
  , GMergeable(..)
    -- * Documentation
  , generateExampleYaml
  , generateFullYaml
    -- * Schema definitions
  , librarySchema
  , systemSchema
  , serverSchema
  , downloadSchema
  , indexerSchema
  , musicbrainzSchema
  , mediaSchema
  , notificationSchema
  , allSchemas
  ) where

import GHC.Generics
import qualified Data.Text as T
import Data.Aeson (ToJSON(..), object, (.=), Value)
import qualified Data.Aeson.Key as Key

-- =============================================================================
-- Field Metadata
-- =============================================================================

-- | Field type for UI rendering
data FieldType
  = FTString        -- ^ Text input
  | FTPath          -- ^ Path picker
  | FTInt           -- ^ Integer input
  | FTBool          -- ^ Toggle/checkbox
  | FTEnum [Text]   -- ^ Dropdown with options
  | FTList          -- ^ List of items
  | FTObject        -- ^ Complex nested object
  deriving (Show, Eq)

instance ToJSON FieldType where
  toJSON FTString = object ["type" .= ("string" :: Text)]
  toJSON FTPath = object ["type" .= ("path" :: Text)]
  toJSON FTInt = object ["type" .= ("integer" :: Text)]
  toJSON FTBool = object ["type" .= ("boolean" :: Text)]
  toJSON (FTEnum opts) = object ["type" .= ("enum" :: Text), "options" .= opts]
  toJSON FTList = object ["type" .= ("list" :: Text)]
  toJSON FTObject = object ["type" .= ("object" :: Text)]

-- | Metadata for a config field
data FieldMeta = FieldMeta
  { fmName :: Text           -- ^ Field name (snake_case for TOML)
  , fmHelp :: Text           -- ^ Help text
  , fmType :: FieldType      -- ^ Field type for UI rendering
  , fmEnvVar :: Maybe Text   -- ^ Environment variable name
  , fmCliFlag :: Maybe Text  -- ^ CLI long flag
  , fmCliShort :: Maybe Char -- ^ CLI short flag
  , fmExample :: Maybe Text  -- ^ Example value
  , fmSensitive :: Bool      -- ^ Hide in logs/output
  , fmRequired :: Bool       -- ^ Is this field required?
  } deriving (Show, Eq)

instance ToJSON FieldMeta where
  toJSON fm = object $ catMaybes
    [ Just $ "name" .= fmName fm
    , Just $ "description" .= fmHelp fm
    , Just $ "type" .= fmType fm
    , ("env_var" .=) <$> fmEnvVar fm
    , ("example" .=) <$> fmExample fm
    , if fmSensitive fm then Just $ "sensitive" .= True else Nothing
    , if fmRequired fm then Just $ "required" .= True else Nothing
    ]

-- | Create a field with help text (defaults to string type)
(.::) :: Text -> Text -> FieldMeta
name .:: helpText = FieldMeta
  { fmName = name
  , fmHelp = helpText
  , fmType = FTString
  , fmEnvVar = Nothing
  , fmCliFlag = Nothing
  , fmCliShort = Nothing
  , fmExample = Nothing
  , fmSensitive = False
  , fmRequired = False
  }

infixl 8 .::

-- | Add environment variable
envVar :: Text -> FieldMeta -> FieldMeta
envVar var fm = fm { fmEnvVar = Just var }

-- | Add CLI flag
cliFlag :: Text -> FieldMeta -> FieldMeta
cliFlag flag fm = fm { fmCliFlag = Just flag }

-- | Add CLI short flag
cliShort :: Char -> FieldMeta -> FieldMeta
cliShort c fm = fm { fmCliShort = Just c }

-- | Add example value
example :: Text -> FieldMeta -> FieldMeta
example ex fm = fm { fmExample = Just ex }

-- | Mark as sensitive
sensitive :: FieldMeta -> FieldMeta
sensitive fm = fm { fmSensitive = True }

-- | Mark as required
required :: FieldMeta -> FieldMeta
required fm = fm { fmRequired = True }

-- | Set field type
fieldType :: FieldType -> FieldMeta -> FieldMeta
fieldType ft fm = fm { fmType = ft }

-- | Convenience: mark as path type
pathField :: FieldMeta -> FieldMeta
pathField = fieldType FTPath

-- | Convenience: mark as boolean type
boolField :: FieldMeta -> FieldMeta
boolField = fieldType FTBool

-- | Convenience: mark as integer type
intField :: FieldMeta -> FieldMeta
intField = fieldType FTInt

-- | Convenience: mark as enum type
enumField :: [Text] -> FieldMeta -> FieldMeta
enumField opts = fieldType (FTEnum opts)

-- =============================================================================
-- Schema
-- =============================================================================

-- | A config section schema
data Schema = Schema
  { schemaSection :: Text      -- ^ Section name (e.g., "library")
  , schemaHelp :: Text         -- ^ Section description
  , schemaFields :: [FieldMeta] -- ^ Field metadata
  } deriving (Show, Eq)

instance ToJSON Schema where
  toJSON s = object
    [ "section" .= schemaSection s
    , "description" .= schemaHelp s
    , "fields" .= schemaFields s
    ]

-- | Create a schema
schema :: Text -> Text -> [FieldMeta] -> Schema
schema = Schema

-- | Full schema as JSON (for /api/config/schema endpoint)
schemaToJSON :: [Schema] -> Value
schemaToJSON schemas = object
  [ Key.fromText (schemaSection s) .= object
      [ "description" .= schemaHelp s
      , "fields" .= schemaFields s
      ]
  | s <- schemas
  ]

-- =============================================================================
-- Default values via Generic
-- =============================================================================

class Default a where
  def :: a
  default def :: (Generic a, GDefault (Rep a)) => a
  def = to gDef

class GDefault f where
  gDef :: f p

instance GDefault U1 where
  gDef = U1

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
  gDef = gDef :*: gDef

instance GDefault a => GDefault (M1 i c a) where
  gDef = M1 gDef

instance Default a => GDefault (K1 i a) where
  gDef = K1 def

-- Base instances
instance Default Bool where def = False
instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Text where def = ""
instance Default String where def = ""
instance Default (Maybe a) where def = Nothing
instance Default [a] where def = []

-- =============================================================================
-- Merging via Generic (second wins)
-- =============================================================================

class Mergeable a where
  -- | Merge two configs. For Maybe fields, Just wins over Nothing.
  -- For other fields, second wins.
  merge :: a -> a -> a
  default merge :: (Generic a, GMergeable (Rep a)) => a -> a -> a
  merge a b = to $ gMerge (from a) (from b)

class GMergeable f where
  gMerge :: f p -> f p -> f p

instance GMergeable U1 where
  gMerge _ _ = U1

instance (GMergeable a, GMergeable b) => GMergeable (a :*: b) where
  gMerge (a1 :*: b1) (a2 :*: b2) = gMerge a1 a2 :*: gMerge b1 b2

instance GMergeable a => GMergeable (M1 i c a) where
  gMerge (M1 a) (M1 b) = M1 (gMerge a b)

-- For Maybe, prefer Just
instance GMergeable (K1 i (Maybe a)) where
  gMerge (K1 Nothing) (K1 b) = K1 b
  gMerge (K1 a) (K1 Nothing) = K1 a
  gMerge _ (K1 b) = K1 b  -- Both Just: take second

-- For other types, second wins
instance {-# OVERLAPPABLE #-} GMergeable (K1 i a) where
  gMerge _ (K1 b) = K1 b

-- =============================================================================
-- Environment variable loading
-- =============================================================================

-- | Type class for loading a field from a string
class FieldLoader a where
  loadField :: Text -> Maybe a

instance FieldLoader Text where
  loadField = Just

instance FieldLoader String where
  loadField = Just . toString

instance FieldLoader Int where
  loadField = readMaybe . toString

instance FieldLoader Bool where
  loadField t = case T.toLower t of
    "true" -> Just True
    "1" -> Just True
    "yes" -> Just True
    "false" -> Just False
    "0" -> Just False
    "no" -> Just False
    _ -> Nothing

instance FieldLoader a => FieldLoader (Maybe a) where
  loadField t
    | T.null t = Just Nothing
    | otherwise = Just <$> loadField t

-- =============================================================================
-- Documentation generation
-- =============================================================================

-- | Generate example YAML for a single schema section
generateExampleYaml :: Schema -> Text
generateExampleYaml s = T.unlines $
  [ schemaSection s <> ":"
  , "  # " <> schemaHelp s
  ] ++ concatMap fieldToYaml (schemaFields s)
  where
    fieldToYaml :: FieldMeta -> [Text]
    fieldToYaml fm =
      [ "  # " <> fmHelp fm
      ] ++ envComment fm ++
      [ "  " <> fmName fm <> ": " <> valueExample fm
      ]

    envComment :: FieldMeta -> [Text]
    envComment fm = case fmEnvVar fm of
      Nothing -> []
      Just var -> ["  # Env: " <> var]

    valueExample :: FieldMeta -> Text
    valueExample fm = fromMaybe "null" (fmExample fm)

-- | Generate complete example YAML for all schemas
generateFullYaml :: Text
generateFullYaml = T.unlines $
  [ "# Skema Configuration File"
  , "#"
  , "# Generated from Skema.Config.Schema"
  , ""
  ] ++ intersperse "" (map generateExampleYaml allSchemas)

-- =============================================================================
-- Schema definitions (SINGLE SOURCE OF TRUTH for metadata)
-- =============================================================================

-- | Library configuration schema
librarySchema :: Schema
librarySchema = schema "library" "Music library configuration"
  [ "path" .:: "Path to music library directory"
      & pathField
      & example "/path/to/music"
  , "watch" .:: "Watch library directory for changes"
      & boolField
      & example "true"
  , "auto_scan" .:: "Automatically scan library on interval"
      & boolField
      & example "false"
  , "auto_scan_interval_mins" .:: "Minutes between automatic scans"
      & intField
      & example "60"
  , "auto_scan_on_startup" .:: "Scan library when application starts"
      & boolField
      & example "true"
  , "normalize_featuring" .:: "Normalize featuring artist format in track titles"
      & boolField
      & example "false"
  , "normalize_featuring_to" .:: "Format to use for featuring artists"
      & example "feat."
  , "path_format" .:: "Directory structure format for organizing files"
      & example "{album_artist}/{year} - {album}/"
  , "file_format" .:: "File naming format"
      & example "{if:multidisc|{disc:02}-}{track:02} - {title}.{ext}"
  ]

-- | System configuration schema
systemSchema :: Schema
systemSchema = schema "system" "System and paths configuration"
  [ "watch_config_file" .:: "Watch config file for changes and reload automatically"
      & boolField
      & example "true"
  , "database_path" .:: "SQLite database file path"
      & pathField
      & example "skema.db"
  , "data_dir" .:: "Data directory override (default: platform-specific)"
      & pathField
      & example "~/.local/share/skema"
  , "cache_dir" .:: "Cache directory override (default: platform-specific)"
      & pathField
      & example "~/.cache/skema"
  , "state_dir" .:: "State directory override (default: platform-specific)"
      & pathField
      & example "~/.local/state/skema"
  ]

-- | Server configuration schema
serverSchema :: Schema
serverSchema = schema "server" "HTTP server configuration"
  [ "host" .:: "Server bind address"
      & example "0.0.0.0"
  , "port" .:: "Server port"
      & intField
      & example "8182"
  , "username" .:: "Username for API authentication"
      & example "admin"
  , "password" .:: "Password for API authentication (will be bcrypt hashed)"
      & sensitive
      & example "your-secure-password"
  , "jwt_secret" .:: "JWT signing secret (auto-generated if not provided)"
      & sensitive
  , "jwt_expiration_hours" .:: "JWT token expiration time in hours"
      & intField
      & example "168"
  ]

-- | Download configuration schema
downloadSchema :: Schema
downloadSchema = schema "download" "Download client configuration"
  [ "nzb_client" .:: "NZB download client configuration"
      & fieldType FTObject
      & example "{ type = \"sabnzbd\", url = \"http://localhost:8080\", api_key = \"...\" }"
  , "torrent_client" .:: "Torrent download client configuration"
      & fieldType FTObject
      & example "{ type = \"transmission\", url = \"http://localhost:9091\" }"
  , "directory" .:: "Directory for completed downloads (before import)"
      & pathField
      & example "./downloads"
  , "check_interval" .:: "How often to check for completed downloads (in seconds)"
      & intField
      & example "60"
  , "auto_import" .:: "Automatically import completed downloads"
      & boolField
      & example "true"
  , "min_seeders" .:: "Minimum seeders for torrents (null = no minimum)"
      & intField
      & example "5"
  , "max_size" .:: "Maximum download size in MB (null = no limit)"
      & intField
      & example "1000"
  ]

-- | Indexer configuration schema
indexerSchema :: Schema
indexerSchema = schema "indexers" "Usenet/torrent indexer configuration"
  [ "list" .:: "List of configured indexers"
      & fieldType FTList
      & example "[]"
  , "search_timeout" .:: "Search timeout per indexer in seconds"
      & intField
      & example "30"
  ]

-- | MusicBrainz configuration schema
musicbrainzSchema :: Schema
musicbrainzSchema = schema "musicbrainz" "MusicBrainz metadata provider configuration"
  [ "server" .:: "Which MusicBrainz server to use"
      & enumField ["official", "headphones_vip"]
      & example "official"
  , "username" .:: "Username for Headphones VIP (required if using VIP)"
      & example "myuser"
  , "password" .:: "Password for Headphones VIP (required if using VIP)"
      & sensitive
  , "album_types" .:: "Primary album types to fetch (e.g., Album, EP)"
      & fieldType FTList
      & example "[\"Album\"]"
  , "exclude_secondary_types" .:: "Secondary types to exclude (e.g., Live, Compilation)"
      & fieldType FTList
      & example "[\"Live\", \"Compilation\"]"
  ]

-- | Media (artwork/metadata) configuration schema
mediaSchema :: Schema
mediaSchema = schema "media" "Media providers configuration"
  [ "lastfm_api_key" .:: "Last.fm API key for artist images and scrobbling"
      & sensitive
      & example "your-lastfm-api-key"
  ]

-- | Notification configuration schema
notificationSchema :: Schema
notificationSchema = schema "notifications" "Notification configuration"
  [ "enabled" .:: "Enable/disable all notifications"
      & boolField
      & example "false"
  , "providers" .:: "List of notification providers"
      & fieldType FTList
      & example "[]"
  , "on_album_found" .:: "Send notification when new wanted albums are found"
      & boolField
      & example "true"
  , "on_album_downloaded" .:: "Send notification when albums are downloaded"
      & boolField
      & example "true"
  , "on_album_imported" .:: "Send notification when albums are imported to library"
      & boolField
      & example "true"
  ]

-- | All config schemas
allSchemas :: [Schema]
allSchemas =
  [ librarySchema
  , systemSchema
  , serverSchema
  , downloadSchema
  , indexerSchema
  , musicbrainzSchema
  , mediaSchema
  , notificationSchema
  ]
