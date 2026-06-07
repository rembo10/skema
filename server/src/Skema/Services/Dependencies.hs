{-# LANGUAGE DeriveGeneric #-}

-- | Service-specific dependency types.
--
-- Instead of passing a monolithic ServiceContext to all services,
-- each service declares its specific dependencies. This makes it clear
-- what each service needs and allows for better testing and modularity.
module Skema.Services.Dependencies
  ( -- * Scanner Dependencies
    ScannerDeps(..)
    -- * Identifier Dependencies
  , IdentifierDeps(..)
    -- * Grouper Dependencies
  , GrouperDeps(..)
    -- * Download Dependencies
  , DownloadDeps(..)
    -- * Image Dependencies
  , ImageDeps(..)
    -- * Importer Dependencies
  , ImporterDeps(..)
    -- * DiffGenerator Dependencies
  , DiffGeneratorDeps(..)
    -- * Persister Dependencies
  , PersisterDeps(..)
    -- * Stats Dependencies
  , StatsDeps(..)
    -- * Catalog Dependencies
  , CatalogDeps(..)
    -- * Acquisition Dependencies
  , AcquisitionDeps(..)
    -- * Thumbnailer Dependencies
  , ThumbnailerDeps(..)
    -- * SourceEvaluator Dependencies
  , SourceEvaluatorDeps(..)
    -- * MetadataWriter Dependencies
  , MetadataWriterDeps(..)
    -- * Notification Dependencies
  , NotificationDeps(..)
  ) where

import Skema.Events.Bus (EventBus)
import Skema.Database.Connection (ConnectionPool)
import Skema.Config.Types (Config)
import Skema.MusicBrainz.Client (MBClientEnv)
import Skema.HTTP.Client (HttpClient)
import Skema.Clock (Clock)
import Katip (LogEnv)

-- | Dependencies for the Scanner service.
-- Scans the library filesystem and publishes LibraryScanned events.
data ScannerDeps = ScannerDeps
  { scanEventBus :: EventBus
  , scanLogEnv :: LogEnv
  , scanDbPool :: ConnectionPool
  }

-- | Dependencies for the Identifier service.
-- Identifies albums using MusicBrainz and publishes identification events.
data IdentifierDeps = IdentifierDeps
  { identEventBus :: EventBus
  , identLogEnv :: LogEnv
  , identDbPool :: ConnectionPool
  , identConfigVar :: TVar Config
  , identMBClient :: MBClientEnv
  , identClock :: Clock
  }

-- | Dependencies for the Grouper service.
-- Groups scanned files into album clusters.
data GrouperDeps = GrouperDeps
  { groupEventBus :: EventBus
  , groupLogEnv :: LogEnv
  , groupDbPool :: ConnectionPool
  , groupConfigVar :: TVar Config
  , groupClock :: Clock
  }

-- | Dependencies for the Download service.
-- Handles torrent/NZB downloads and progress monitoring.
data DownloadDeps = DownloadDeps
  { dlEventBus :: EventBus
  , dlLogEnv :: LogEnv
  , dlDbPool :: ConnectionPool
  , dlConfigVar :: TVar Config
  , dlHttpClient :: HttpClient
  , dlProgressMap :: TVar (Map Int64 (Double, Text))
  , dlClock :: Clock
  }

-- | Dependencies for the Image service.
-- Fetches and caches album artwork.
data ImageDeps = ImageDeps
  { imgEventBus :: EventBus
  , imgLogEnv :: LogEnv
  , imgDbPool :: ConnectionPool
  , imgConfigVar :: TVar Config
  , imgHttpClient :: HttpClient
  , imgCacheDir :: FilePath
  }

-- | Dependencies for the Importer service.
-- Imports completed downloads into the library.
data ImporterDeps = ImporterDeps
  { impEventBus :: EventBus
  , impLogEnv :: LogEnv
  , impDbPool :: ConnectionPool
  , impConfigVar :: TVar Config
  , impMBClient :: MBClientEnv
  , impClock :: Clock
  }

-- | Dependencies for the DiffGenerator service.
-- Generates metadata differences between file tags and MusicBrainz data.
data DiffGeneratorDeps = DiffGeneratorDeps
  { diffEventBus :: EventBus
  , diffLogEnv :: LogEnv
  , diffDbPool :: ConnectionPool
  , diffMBClient :: MBClientEnv
  }

-- | Dependencies for the Persister service.
-- Persists metadata changes to audio files.
data PersisterDeps = PersisterDeps
  { persistEventBus :: EventBus
  , persistLogEnv :: LogEnv
  , persistDbPool :: ConnectionPool
  }

-- | Dependencies for the Stats service.
-- Updates library statistics.
data StatsDeps = StatsDeps
  { statsEventBus :: EventBus
  , statsLogEnv :: LogEnv
  , statsDbPool :: ConnectionPool
  }

-- | Dependencies for the Catalog service.
-- Manages the catalog of artists and albums.
data CatalogDeps = CatalogDeps
  { catEventBus :: EventBus
  , catLogEnv :: LogEnv
  , catDbPool :: ConnectionPool
  , catConfigVar :: TVar Config
  , catMBClient :: MBClientEnv
  , catHttpClient :: HttpClient
  , catCacheDir :: FilePath
  , catClock :: Clock
  }

-- | Dependencies for the Acquisition service.
-- Manages acquisition rules and automatic downloads.
data AcquisitionDeps = AcquisitionDeps
  { acqEventBus :: EventBus
  , acqLogEnv :: LogEnv
  , acqDbPool :: ConnectionPool
  , acqConfigVar :: TVar Config
  , acqMBClient :: MBClientEnv
  , acqClock :: Clock
  }

-- | Dependencies for the Thumbnailer service.
-- Generates thumbnails from full-size images.
data ThumbnailerDeps = ThumbnailerDeps
  { thumbEventBus :: EventBus
  , thumbLogEnv :: LogEnv
  , thumbDbPool :: ConnectionPool
  , thumbCacheDir :: FilePath
  }

-- | Dependencies for the SourceEvaluator service.
-- Periodically evaluates acquisition sources against providers.
data SourceEvaluatorDeps = SourceEvaluatorDeps
  { sourceEvalEventBus :: EventBus
  , sourceEvalLogEnv :: LogEnv
  , sourceEvalDbPool :: ConnectionPool
  , sourceEvalMBClient :: MBClientEnv
  , sourceEvalHttpClient :: HttpClient
  }

-- | Dependencies for the MetadataWriter service.
-- Writes metadata changes to audio files asynchronously.
data MetadataWriterDeps = MetadataWriterDeps
  { writerEventBus :: EventBus
  , writerLogEnv :: LogEnv
  , writerDbPool :: ConnectionPool
  }

-- | Dependencies for the Notification service.
-- Sends notifications through configured providers (Pushover, etc.).
data NotificationDeps = NotificationDeps
  { notifEventBus :: EventBus
  , notifConfigVar :: TVar Config
  , notifHttpClient :: HttpClient
  , notifLogEnv :: LogEnv
  , notifDbPool :: ConnectionPool
  , notifClock :: Clock
  }
