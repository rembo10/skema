{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test environment setup helpers.
--
-- Provides utilities for creating test databases, event buses,
-- and complete service contexts for integration testing.
module Helpers.TestEnv
  ( -- * Test Environment
    TestEnv(..)
  , withTestEnv
    -- * Service Context Builders
  , mkTestServiceContext
  ) where

import Skema.Database.Connection (ConnectionPool, createConnectionPool, destroyConnectionPool)
import Skema.Database.Types (DatabaseConfig(..))
import Skema.Database.Migrations (runMigrations)
import Skema.Events.Bus (EventBus, newEventBus)
import Skema.Services.Types (ServiceContext(..))
import Skema.HTTP.Client (HttpClient, newHttpClient, defaultHttpConfig, defaultUserAgentData)
import Skema.MusicBrainz.Client (MBClientEnv, newMBClientEnv)
import Skema.Config.Types (Config, musicbrainz, defaultConfig)
import Control.Exception (bracket)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import Katip
import qualified Control.Concurrent.STM as STM

-- | Test environment with all necessary components.
data TestEnv = TestEnv
  { tePool :: ConnectionPool
    -- ^ Database connection pool (in-memory SQLite)
  , teEventBus :: EventBus
    -- ^ Event bus for publishing/subscribing
  , teLogEnv :: LogEnv
    -- ^ Logging environment
  , teConfigVar :: TVar Config
    -- ^ Configuration (can be modified in tests)
  , teHttpClient :: HttpClient
    -- ^ HTTP client
  , teMBClientEnv :: MBClientEnv
    -- ^ MusicBrainz client environment
  , teCacheDir :: FilePath
    -- ^ Temporary cache directory
  }

-- | Create a test environment with real HTTP client.
--
-- Uses in-memory SQLite database and temporary cache directory.
-- Automatically runs migrations and sets up event bus.
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action = do
  -- Create minimal LogEnv for testing (only show errors and critical messages)
  handleScribe <- mkHandleScribe ColorIfTerminal stderr (permitItem ErrorS) V2
  let mkLogEnv = registerScribe "stderr" handleScribe defaultScribeSettings =<< initLogEnv "skema-test" "test"

  bracket mkLogEnv closeScribes $ \le -> do
    -- Use temporary SQLite file for tests
    withSystemTempFile "skema-test.db" $ \tmpFile _handle -> do
      let dbConfig = DatabaseConfig
            { dbPath = tmpFile
            , dbPoolSize = 4  -- Small pool for tests
            }

      -- Create connection pool and run migrations
      bracket (createConnectionPool le dbConfig) destroyConnectionPool $ \pool -> do
        runMigrations le pool

        -- Create temporary cache directory
        withSystemTempDirectory "skema-cache" $ \cacheDir -> do
          -- Create event bus
          bus <- newEventBus

          -- Create default config
          configVar <- STM.newTVarIO defaultConfig

          -- Create HTTP client
          httpClient <- newHttpClient le defaultHttpConfig defaultUserAgentData

          -- Read config for MusicBrainz setup
          config <- STM.readTVarIO configVar
          let mbConfig = musicbrainz config
          let mbClientEnv = newMBClientEnv httpClient mbConfig

          let env = TestEnv
                { tePool = pool
                , teEventBus = bus
                , teLogEnv = le
                , teConfigVar = configVar
                , teHttpClient = httpClient
                , teMBClientEnv = mbClientEnv
                , teCacheDir = cacheDir
                }

          action env

-- | Create a ServiceContext from a TestEnv.
mkTestServiceContext :: TestEnv -> ServiceContext
mkTestServiceContext TestEnv{..} = ServiceContext
  { scEventBus = teEventBus
  , scDbPool = tePool
  , scConfigVar = teConfigVar
  , scLogEnv = teLogEnv
  , scHttpClient = teHttpClient
  , scMBClientEnv = teMBClientEnv
  , scCacheDir = teCacheDir
  , scDownloadProgressMap = error "Download progress map not initialized in test context - create one with newTVarIO Map.empty if needed"
  }
