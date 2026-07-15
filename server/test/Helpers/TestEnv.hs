{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Test environment setup helpers.
--
-- Provides utilities for creating test databases, event buses,
-- and complete service contexts for integration testing.
--
-- HTTP is mocked by default: 'withTestEnv' installs a transport that
-- throws on any request. Tests that need HTTP must pass matchers via
-- 'withTestEnvMock'.
module Helpers.TestEnv
  ( -- * Test Environment
    TestEnv(..)
  , withTestEnv
  , withTestEnvMock
  , withTestEnvTransport
    -- * Service Context Builders
  , mkTestServiceContext
  ) where

import Skema.Database.Connection (ConnectionPool, createConnectionPool, destroyConnectionPool)
import Skema.Database.Types (DatabaseConfig(..))
import Skema.Database.Migrations (runMigrations)
import Skema.Events.Bus (EventBus, newEventBus)
import Skema.Services.Types (ServiceContext(..))
import Skema.HTTP.Client
  ( HttpClient
  , HttpTransport
  , newHttpClientWithTransport
  , defaultHttpConfig
  , defaultUserAgentData
  )
import Skema.MusicBrainz.Client (MBClientEnv, newMBClientEnv)
import Skema.Clock (Clock, systemClock)
import Skema.Config.Types (Config, musicbrainz, defaultConfig)
import Helpers.MockHttp (RequestMatcher, mockTransport)
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
  , teClock :: Clock
    -- ^ Source of the current time. Defaults to the real system clock;
    -- override per test (e.g. with 'Skema.Clock.fixedClock') for determinism.
  , teProgressMap :: TVar (Map Int64 (Double, Text))
    -- ^ In-memory download progress map (real TVar, safe to read/write in tests)
  }

-- | Create a test environment with a strict mock HTTP transport.
--
-- Any outbound HTTP request fails with 'UnexpectedHttpRequest'. Tests that
-- need to serve canned responses should use 'withTestEnvMock' instead.
--
-- Uses in-memory SQLite database and temporary cache directory.
-- Automatically runs migrations and sets up event bus.
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv = withTestEnvMock []

-- | Create a test environment with a mock HTTP transport driven by the
-- given matchers. Matchers are tried in order; the first to match wins.
-- Requests with no matching matcher throw 'UnexpectedHttpRequest'.
withTestEnvMock :: [RequestMatcher] -> (TestEnv -> IO a) -> IO a
withTestEnvMock matchers = withTestEnvTransport (mockTransport matchers)

-- | Create a test environment backed by an arbitrary HTTP transport.
--
-- Most tests want 'withTestEnvMock' (canned responses). Use this directly
-- when the transport itself matters — e.g. to introduce latency and exercise
-- concurrency/streaming ordering.
withTestEnvTransport :: HttpTransport -> (TestEnv -> IO a) -> IO a
withTestEnvTransport transport action = do
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

          -- Create in-memory download progress map
          progressMap <- STM.newTVarIO mempty

          -- Create HTTP client with the provided transport
          httpClient <- newHttpClientWithTransport le defaultHttpConfig defaultUserAgentData transport

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
                , teClock = systemClock
                , teProgressMap = progressMap
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
  , scDownloadProgressMap = teProgressMap
  , scClock = teClock
  }
