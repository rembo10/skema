{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | ConfigSchema instances for all config types.
--
-- SINGLE SOURCE OF TRUTH for configuration structure.
-- To add a new field:
--   1. Add it to the data type in Config.Types
--   2. Add it to the configFields list here
--   3. Update configToJSON/configFromJSON
--   4. Everything else derives automatically!
--
-- Note: Orphan instances are intentional in this module - it exists specifically
-- to centralize all ConfigSchema instances for better maintainability.
module Skema.Config.SchemaInstances () where

import Skema.Config.Types
import Skema.Config.SchemaClass
import Data.Aeson (object, (.=), (.:?), (.!=), withObject)
import qualified System.OsPath as OP
import System.IO.Unsafe (unsafePerformIO)

-- | LibraryConfig schema instance
instance ConfigSchema LibraryConfig where
  configSection _ = "library"

  configDescription _ = "Music library configuration"

  configFields _ =
    [ FieldInfo
        { fieldName = "path"
        , fieldDescription = "Path to your music library"
        , fieldType = FOptional FPath
        , fieldDefault = Nothing
        , fieldExample = Just "/home/user/Music"
        }
    , FieldInfo
        { fieldName = "watch"
        , fieldDescription = "Enable file system watching for real-time updates"
        , fieldType = FBool
        , fieldDefault = Just "true"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "auto_scan"
        , fieldDescription = "Enable automatic periodic scanning"
        , fieldType = FBool
        , fieldDefault = Just "true"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "auto_scan_interval_mins"
        , fieldDescription = "Interval between automatic scans in minutes"
        , fieldType = FInt
        , fieldDefault = Just "60"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "auto_scan_on_startup"
        , fieldDescription = "Run a full scan on application startup"
        , fieldType = FBool
        , fieldDefault = Just "true"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "normalize_featuring"
        , fieldDescription = "Normalize \"featuring\" join phrases in artist credits"
        , fieldType = FBool
        , fieldDefault = Just "false"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "normalize_featuring_to"
        , fieldDescription = "What to normalize \"featuring\" to (e.g., \"feat.\", \"ft.\")"
        , fieldType = FText
        , fieldDefault = Just "feat."
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "path_format"
        , fieldDescription = "Template for album directory paths (e.g., \"{album_artist}/{year} - {album}/\")"
        , fieldType = FText
        , fieldDefault = Just "{album_artist}/{year} - {album}/"
        , fieldExample = Just "{if:lossless|Lossless/|Lossy/}{album_artist}/{year} - {album}/"
        }
    , FieldInfo
        { fieldName = "file_format"
        , fieldDescription = "Template for track filenames (e.g., \"{if:multidisc|{disc:02}-}{track:02} - {title}.{ext}\")"
        , fieldType = FText
        , fieldDefault = Just "{if:multidisc|{disc:02}-}{track:02} - {title}.{ext}"
        , fieldExample = Just "{track:02}. {artist} - {title}.{ext}"
        }
    ]

  configDefault = defaultLibraryConfig

  configToJSON cfg =
    let pathField = case libraryPath cfg of
          Nothing -> []
          Just path -> ["path" .= (unsafePerformIO $ OP.decodeUtf path :: String)]
     in object $ pathField <>
          [ "watch" .= libraryWatch cfg
          , "auto_scan" .= libraryAutoScan cfg
          , "auto_scan_interval_mins" .= libraryAutoScanIntervalMins cfg
          , "auto_scan_on_startup" .= libraryAutoScanOnStartup cfg
          , "normalize_featuring" .= libraryNormalizeFeaturing cfg
          , "normalize_featuring_to" .= libraryNormalizeFeaturingTo cfg
          , "path_format" .= libraryPathFormat cfg
          , "file_format" .= libraryFileFormat cfg
          ]

  configFromJSON = withObject "LibraryConfig" $ \o -> do
    maybePath <- o .:? "path"
    watch <- o .:? "watch" .!= True
    autoScan <- o .:? "auto_scan" .!= True
    interval <- o .:? "auto_scan_interval_mins" .!= 60
    scanOnStartup <- o .:? "auto_scan_on_startup" .!= True
    normalizeFeaturing <- o .:? "normalize_featuring" .!= False
    normalizeFeaturingTo <- o .:? "normalize_featuring_to" .!= "feat."
    pathFormat <- o .:? "path_format" .!= "{album_artist}/{year} - {album}/"
    fileFormat <- o .:? "file_format" .!= "{if:multidisc|{disc:02}-}{track:02} - {title}.{ext}"
    let osPath = case maybePath of
          Nothing -> Nothing
          Just (pathStr :: String) -> Just $ unsafePerformIO $ OP.encodeUtf pathStr
    pure $ LibraryConfig osPath watch autoScan interval scanOnStartup normalizeFeaturing normalizeFeaturingTo pathFormat fileFormat

-- | SystemConfig schema instance
instance ConfigSchema SystemConfig where
  configSection _ = "system"

  configDescription _ = "System-level configuration"

  configFields _ =
    [ FieldInfo
        { fieldName = "watch_config_file"
        , fieldDescription = "Watch config file for changes and reload automatically"
        , fieldType = FBool
        , fieldDefault = Just "true"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "database_backend"
        , fieldDescription = "Database backend: \"sqlite\" or \"postgresql\""
        , fieldType = FText
        , fieldDefault = Just "sqlite"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "database_path"
        , fieldDescription = "Database path (for sqlite) or connection string (for postgresql)"
        , fieldType = FText
        , fieldDefault = Just "./skema.db"
        , fieldExample = Just "host=localhost port=5432 dbname=skema user=skema password=secret"
        }
    ]

  configDefault = defaultSystemConfig

  configToJSON cfg = object
    [ "watch_config_file" .= systemWatchConfigFile cfg
    , "database_backend" .= systemDatabaseBackend cfg
    , "database_path" .= systemDatabasePath cfg
    , "data_dir" .= systemDataDir cfg
    , "cache_dir" .= systemCacheDir cfg
    , "state_dir" .= systemStateDir cfg
    ]

  configFromJSON = withObject "SystemConfig" $ \o -> do
    watchConfig <- o .:? "watch_config_file" .!= True
    dbBackend <- o .:? "database_backend" .!= "sqlite"
    dbPath <- o .:? "database_path" .!= "./skema.db"
    dataDir <- o .:? "data_dir"
    cacheDir <- o .:? "cache_dir"
    stateDir <- o .:? "state_dir"
    pure $ SystemConfig watchConfig dbBackend dbPath dataDir cacheDir stateDir

-- | ServerConfig schema instance
instance ConfigSchema ServerConfig where
  configSection _ = "server"

  configDescription _ = "HTTP server configuration"

  configFields _ =
    [ FieldInfo
        { fieldName = "host"
        , fieldDescription = "Server host to bind to (use \"0.0.0.0\" to listen on all interfaces)"
        , fieldType = FText
        , fieldDefault = Just "127.0.0.1"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "port"
        , fieldDescription = "HTTP API server port"
        , fieldType = FInt
        , fieldDefault = Just "8181"
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "username"
        , fieldDescription = "Username for API authentication (can be overridden by SKEMA_USERNAME env var)"
        , fieldType = FOptional FText
        , fieldDefault = Nothing
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "password"
        , fieldDescription = "Password for API authentication (can be overridden by SKEMA_PASSWORD env var)"
        , fieldType = FOptional FText
        , fieldDefault = Nothing
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "jwt_secret"
        , fieldDescription = "JWT signing secret (auto-generated if not provided)"
        , fieldType = FOptional FText
        , fieldDefault = Nothing
        , fieldExample = Nothing
        }
    , FieldInfo
        { fieldName = "jwt_expiration_hours"
        , fieldDescription = "JWT token expiration time in hours"
        , fieldType = FInt
        , fieldDefault = Just "24"
        , fieldExample = Nothing
        }
    ]

  configDefault = defaultServerConfig

  configToJSON cfg = object
    [ "host" .= serverHost cfg
    , "port" .= serverPort cfg
    , "username" .= serverUsername cfg
    , "password" .= serverPassword cfg
    , "jwt_secret" .= serverJwtSecret cfg
    , "jwt_expiration_hours" .= serverJwtExpirationHours cfg
    ]

  configFromJSON = withObject "ServerConfig" $ \o -> do
    host <- o .:? "host" .!= "127.0.0.1"
    port <- o .:? "port" .!= 8181
    username <- o .:? "username"
    password <- o .:? "password"
    jwtSecret <- o .:? "jwt_secret"
    jwtExpHours <- o .:? "jwt_expiration_hours" .!= 24
    pure $ ServerConfig host port username password jwtSecret jwtExpHours
