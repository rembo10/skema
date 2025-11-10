{-# LANGUAGE OverloadedStrings #-}

-- | Default configuration values for all config types.
module Skema.Config.Defaults
  ( -- * Default configurations
    defaultConfig
  , defaultLibraryConfig
  , defaultSystemConfig
  , defaultServerConfig
  , defaultDownloadConfig
  , defaultIndexerConfig
  , defaultMusicBrainzConfig
  , defaultMediaConfig
  ) where

import Skema.Config.Types
  ( Config(..)
  , LibraryConfig(..)
  , SystemConfig(..)
  , ServerConfig(..)
  , DownloadConfig(..)
  , IndexerConfig(..)
  , Indexer(..)
  , MusicBrainzConfig(..)
  , MusicBrainzServer(..)
  , MediaConfig(..)
  )

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
  , systemDatabaseBackend = "sqlite"
  , systemDatabasePath = "skema.db"
  , systemDataDir = Nothing      -- Use platform default
  , systemCacheDir = Nothing     -- Use platform default
  , systemStateDir = Nothing     -- Use platform default
  }

-- | Default server configuration.
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverHost = "127.0.0.1"
  , serverPort = 8181
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

-- | Default configuration with all defaults.
defaultConfig :: Config
defaultConfig = Config
  { library = defaultLibraryConfig
  , system = defaultSystemConfig
  , server = defaultServerConfig
  , download = defaultDownloadConfig
  , indexers = defaultIndexerConfig
  , musicbrainz = defaultMusicBrainzConfig
  , media = defaultMediaConfig
  }
