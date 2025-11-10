-- | Common types and context for services.
module Skema.Services.Types
  ( ServiceContext(..)
  ) where

import Skema.Events.Bus (EventBus)
import Skema.Database.Connection (ConnectionPool)
import Skema.Config.Types (Config)
import Skema.MusicBrainz.Client (MBClientEnv)
import Skema.HTTP.Client (HttpClient)
import Katip (LogEnv)

-- | Shared context for all services.
--
-- This provides everything a service needs to operate:
-- - Event bus for publishing and subscribing
-- - Database connection pool
-- - Application configuration (TVar for live updates)
-- - Logging environment
-- - HTTP client (shared for rate limiting across all HTTP calls)
-- - MusicBrainz API client (shared for rate limiting)
-- - Cache directory for storing downloaded images and other cached data
data ServiceContext = ServiceContext
  { scEventBus :: EventBus
  , scDbPool :: ConnectionPool
  , scConfigVar :: TVar Config
    -- ^ Configuration in a TVar for live updates
  , scLogEnv :: LogEnv
  , scHttpClient :: HttpClient
  , scMBClientEnv :: MBClientEnv
  , scCacheDir :: FilePath
  , scDownloadProgressMap :: TVar (Map Int64 (Double, Text))
    -- ^ In-memory map of download progress and status (download_id -> (progress 0.0-1.0, display_status))
  }
