{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Simple, pragmatic config schema system.
--
-- Philosophy:
-- - Record types ARE the source of truth for structure
-- - Metadata is attached via a simple list per section
-- - The schema drives the /api/config/schema endpoint (which the settings
--   UI renders its forms from) and the example YAML
--
-- Usage:
-- @
-- librarySchema :: Schema
-- librarySchema = schema "library" "Library configuration"
--   [ "path"        .:: "Path to music library" & pathField
--   , "watch"       .:: "Watch for file changes" & boolField
--   , "path_format" .:: "Directory structure format"
--   ]
-- @
module Skema.Config.Schema
  ( -- * Field types
    FieldType(..)
    -- * Field metadata
  , FieldMeta(..)
  , (.::)
  , example
  , sensitive
  , required
  , advanced
  , inGroup
  , dependsOn
  , dependsOnValue
  , fieldType
  , pathField
  , urlField
  , boolField
  , intField
  , enumField
    -- * Schema
  , Schema(..)
  , schema
  , schemaToJSON
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
  | FTUrl           -- ^ URL input with protocol selector
  | FTInt           -- ^ Integer input
  | FTBool          -- ^ Toggle/checkbox
  | FTEnum [Text]   -- ^ Dropdown with options
  | FTList          -- ^ List of items
  | FTObject        -- ^ Complex nested object
  deriving (Show, Eq)

instance ToJSON FieldType where
  toJSON FTString = object ["type" .= ("string" :: Text)]
  toJSON FTPath = object ["type" .= ("path" :: Text)]
  toJSON FTUrl = object ["type" .= ("url" :: Text)]
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
  , fmExample :: Maybe Text  -- ^ Default value (shown in YAML)
  , fmSensitive :: Bool      -- ^ Hide in logs/output
  , fmRequired :: Bool       -- ^ Is this field required?
  , fmAdvanced :: Bool       -- ^ Hidden unless "Show Advanced" is toggled
  , fmGroup :: Maybe Text    -- ^ Subsection group heading (Nothing = ungrouped, rendered first)
  , fmDependsOn :: Maybe Text -- ^ Only show if this field is truthy
  , fmDependsOnValue :: Maybe (Text, [Text]) -- ^ Only show if field has one of these values (field, [values])
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
    , if fmAdvanced fm then Just $ "advanced" .= True else Nothing
    , ("group" .=) <$> fmGroup fm
    , ("depends_on" .=) <$> fmDependsOn fm
    , case fmDependsOnValue fm of
        Just (field, values) -> Just $ "depends_on_value" .= object ["field" .= field, "values" .= values]
        Nothing -> Nothing
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
  , fmAdvanced = False
  , fmGroup = Nothing
  , fmDependsOn = Nothing
  , fmDependsOnValue = Nothing
  }

infixl 8 .::

-- | Set default value (shown in YAML)
example :: Text -> FieldMeta -> FieldMeta
example ex fm = fm { fmExample = Just ex }

-- | Mark as sensitive
sensitive :: FieldMeta -> FieldMeta
sensitive fm = fm { fmSensitive = True }

-- | Mark as required
required :: FieldMeta -> FieldMeta
required fm = fm { fmRequired = True }

-- | Mark as advanced (hidden by default)
advanced :: FieldMeta -> FieldMeta
advanced fm = fm { fmAdvanced = True }

-- | Assign the field to a named subsection group (rendered under a heading)
inGroup :: Text -> FieldMeta -> FieldMeta
inGroup name fm = fm { fmGroup = Just name }

-- | Field depends on another field being truthy
dependsOn :: Text -> FieldMeta -> FieldMeta
dependsOn field fm = fm { fmDependsOn = Just field }

-- | Field depends on another field having one of the specified values
dependsOnValue :: Text -> [Text] -> FieldMeta -> FieldMeta
dependsOnValue field values fm = fm { fmDependsOnValue = Just (field, values) }

-- | Set field type
fieldType :: FieldType -> FieldMeta -> FieldMeta
fieldType ft fm = fm { fmType = ft }

-- | Convenience: mark as path type
pathField :: FieldMeta -> FieldMeta
pathField = fieldType FTPath

-- | Convenience: mark as URL type
urlField :: FieldMeta -> FieldMeta
urlField = fieldType FTUrl

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
      -- No example = null default
  -- Scanning
  , "watch" .:: "Watch library directory for changes"
      & boolField
      & example "true"
      & inGroup "Scanning"
  , "auto_scan" .:: "Automatically scan library on interval"
      & boolField
      & example "true"
      & inGroup "Scanning"
  , "auto_scan_interval_mins" .:: "Minutes between automatic scans"
      & intField
      & example "60"
      & inGroup "Scanning"
  , "auto_scan_on_startup" .:: "Scan library when application starts"
      & boolField
      & example "true"
      & inGroup "Scanning"
  -- Importing
  , "auto_upgrade_existing_albums" .:: "Apply the default quality profile to existing library albums when discovered, so they are monitored and upgraded to better releases"
      & boolField
      & example "false"
      & inGroup "Importing"
  -- File Organization
  , "path_format" .:: "Directory structure format for organizing files"
      & example "{album_artist}/{album} [{year}]"
      & inGroup "File Organization"
  , "file_format" .:: "File naming format"
      & example "{track:02} {artist} - {album} {year} - {title}.{ext}"
      & inGroup "File Organization"
  , "normalize_featuring" .:: "Normalize featuring artist format in track titles"
      & boolField
      & example "false"
      & inGroup "File Organization"
  , "normalize_featuring_to" .:: "Format to use for featuring artists"
      & example "feat."
      & dependsOn "normalize_featuring"
      & inGroup "File Organization"
  ]

-- | System configuration schema
systemSchema :: Schema
systemSchema = schema "system" "System and paths configuration"
  [ "watch_config_file" .:: "Watch config file for changes and reload automatically"
      & boolField
      & example "true"
  , "check_updates" .:: "Periodically check GitHub for new Skema releases"
      & boolField
      & example "true"
  -- Paths (advanced, override platform defaults)
  , "database_path" .:: "SQLite database file path (default: skema.db in data directory)"
      & pathField
      & advanced
      & inGroup "Paths"
      -- No example = null, uses default location
  , "data_dir" .:: "Data directory override (default: platform-specific)"
      & pathField
      & advanced
      & inGroup "Paths"
      -- No example = null, uses platform default
  , "cache_dir" .:: "Cache directory override (default: platform-specific)"
      & pathField
      & advanced
      & inGroup "Paths"
      -- No example = null, uses platform default
  ]

-- | Server configuration schema
serverSchema :: Schema
serverSchema = schema "server" "HTTP server configuration"
  [ "host" .:: "Server bind address"
      & example "127.0.0.1"
  , "port" .:: "Server port"
      & intField
      & example "8182"
  , "username" .:: "Username for API authentication (required)"
      -- No example = null, must be set by user
  , "password" .:: "Password for API authentication (required, will be bcrypt hashed)"
      & sensitive
      -- No example = null, must be set by user
  -- API Access (advanced)
  , "api_key" .:: "API key for external access, sent via the X-API-Key header"
      & sensitive
      & advanced
      & inGroup "API Access"
      -- No example = null
  -- Advanced
  , "web_root" .:: "Web root path for hosting at subpaths (e.g., /skema or /)"
      & example "/"
      & advanced
      & inGroup "Advanced"
  , "jwt_secret" .:: "JWT signing secret (auto-generated if not provided)"
      & sensitive
      & advanced
      & inGroup "Advanced"
      -- No example = null, auto-generated
  , "jwt_expiration_hours" .:: "JWT token expiration time in hours"
      & intField
      & example "24"
      & advanced
      & inGroup "Advanced"
  ]

-- | Download configuration schema
downloadSchema :: Schema
downloadSchema = schema "download" "Download client configuration"
  [ "nzb_client" .:: "NZB download client configuration"
      & fieldType FTObject
      -- No example = null, optional
  , "torrent_client" .:: "Torrent download client configuration"
      & fieldType FTObject
      -- No example = null, optional
  , "slskd_client" .:: "slskd (Soulseek) client configuration"
      & fieldType FTObject
      & example (T.unlines
          [ ""
          , "    url: http://localhost:5030"
          , "    api_key: your-api-key"
          , "    enabled: false"
          , "    download_directory: /downloads/slskd"
          , "    min_track_count: 3"
          ])
  , "auto_import" .:: "Automatically import completed downloads"
      & boolField
      & example "true"
  , "replace_library_files" .:: "Delete old library files when upgrading to better quality"
      & boolField
      & example "false"
  , "min_seeders" .:: "Minimum seeders for torrents (null = no minimum)"
      & intField
      & example "1"
  , "max_size_mb" .:: "Maximum download size in MB (null = no limit)"
      & intField
      -- No example = null, no limit
  , "max_search_retries" .:: "Maximum automatic re-search attempts per album after a failed grab"
      & intField
      & example "5"
  , "use_trash" .:: "Move deleted files to trash instead of permanently deleting them"
      & boolField
      & example "true"
  , "trash_retention_days" .:: "Days to keep files in trash before permanent deletion"
      & intField
      & example "7"
  , "check_interval" .:: "How often to check download clients for status updates (in seconds)"
      & intField
      & example "5"
  ]

-- | Indexer configuration schema
indexerSchema :: Schema
indexerSchema = schema "indexers" "Usenet/torrent indexer configuration"
  [ "list" .:: "List of configured indexers"
      & fieldType FTList
      & example (T.unlines
          [ ""
          , "    - name: Bullet"
          , "      url: https://bullet.codeshy.com"
          , "      api_key: your-api-key"
          , "      enabled: false"
          , "      priority: 0"
          , "      categories: [3000, 3010]"
          ])
  , "search_timeout" .:: "Search timeout per indexer in seconds"
      & intField
      & example "30"
  , "max_results_per_indexer" .:: "Maximum number of results to request per indexer search"
      & intField
      & example "50"
  ]

-- | MusicBrainz configuration schema
musicbrainzSchema :: Schema
musicbrainzSchema = schema "musicbrainz" "MusicBrainz metadata provider configuration"
  [ "server" .:: "Which MusicBrainz server to use"
      & enumField ["official", "headphones_vip", "custom"]
      & example "official"
  , "custom_url" .:: "Custom MusicBrainz server URL (required if server is 'custom')"
      & urlField
      & dependsOnValue "server" ["custom"]
      & example "https://musicbrainz.example.com"
  , "username" .:: "Username for authentication (required for Headphones VIP, optional for custom)"
      & dependsOnValue "server" ["headphones_vip", "custom"]
      -- No example = null
  , "password" .:: "Password for authentication (required for Headphones VIP, optional for custom)"
      & sensitive
      & dependsOnValue "server" ["headphones_vip", "custom"]
      -- No example = null
  , "album_types" .:: "Primary album types to fetch (e.g., Album, EP)"
      & fieldType FTList
      & example "[\"Album\"]"
  , "exclude_secondary_types" .:: "Secondary types to exclude (e.g., Live, Compilation)"
      & fieldType FTList
      & example "[\"Live\", \"Compilation\"]"
  -- Matching (advanced, tuning knobs)
  , "match_min_confidence" .:: "Minimum match confidence (%) required to auto-accept a MusicBrainz match"
      & intField
      & example "35"
      & advanced
      & inGroup "Matching"
  , "max_candidates" .:: "Maximum number of release candidates to consider per album"
      & intField
      & example "5"
      & advanced
      & inGroup "Matching"
  , "search_limit" .:: "Maximum number of releases to fetch from MusicBrainz search"
      & intField
      & example "20"
      & advanced
      & inGroup "Matching"
  ]

-- | Media (artwork/metadata) configuration schema
mediaSchema :: Schema
mediaSchema = schema "media" "Media providers configuration"
  [ "lastfm_api_key" .:: "Last.fm API key for artist images and scrobbling"
      & sensitive
      -- No example = null
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
