{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression tests for streaming indexer release search.
--
-- Guards the bug where 'streamSearchResults' spawned each indexer search with
-- a fire-and-forget @async@ and then wrote @SearchDone@ immediately. Because
-- the SSE event loop stops at @SearchDone@, the stream closed before any
-- @ReleaseFound@ reached the consumer, so every search returned zero releases.
--
-- The indexer HTTP response is deliberately delayed: with the bug, @SearchDone@
-- is written before the (delayed) release ever arrives, so the consumer sees no
-- releases and the test fails deterministically. With the fix (waiting for the
-- searches before signalling done), the releases always arrive first.
module Integration.IndexerStreamingSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Servant (Handler, (:<|>)(..))
import Servant.Types.SourceT (SourceT(..), StepT(..))

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T

import Helpers.TestEnv (TestEnv(..), withTestEnvTransport)
import Helpers.Handler (runOk)
import Helpers.Builders (seedCatalogAlbum)
import Helpers.MockHttp (mockTransport, respond, methodIs, hostIs, textResponse, (.&.))

import Skema.API.Handlers.Catalog (catalogServer)
import Skema.API.Types.Catalog (ReleaseStreamEvent(..), AlbumReleasesResponse(..))
import Skema.API.Types.Common (SourceIO)
import Skema.Database.Connection (withConnection)
import Skema.HTTP.Client (HttpTransport(..))
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.Services.TaskManager (newTaskManager)
import Skema.Auth.JWT (getJWTSecret, generateJWTSecretString)
import qualified Skema.Config.Types as Cfg

tests :: TestTree
tests = testGroup "Integration.IndexerStreaming"
  [ testCase "streaming search delivers releases before SearchDone" testStreamsReleasesBeforeDone
  , testCase "streaming search populates the release cache" testStreamingPopulatesCache
  ]

-- Fixture data --------------------------------------------------------------

-- | A minimal Newznab JSON response carrying a single release.
newznabJson :: Text
newznabJson = T.concat
  [ "{\"channel\":{\"item\":{"
  , "\"title\":\"Test Artist - Test Album 2020 FLAC\","
  , "\"link\":\"http://mock.indexer/dl/1\","
  , "\"size\":123456789,"
  , "\"enclosure\":{\"@attributes\":{"
  , "\"url\":\"http://mock.indexer/file.nzb\","
  , "\"length\":123456789,"
  , "\"type\":\"application/x-nzb\"}}"
  , "}}}"
  ]

-- | An enabled indexer whose requests the mock transport answers.
mockIndexer :: Cfg.Indexer
mockIndexer = Cfg.Indexer
  { Cfg.indexerName = "MockIndexer"
  , Cfg.indexerUrl = "http://mock.indexer"
  , Cfg.indexerApiKey = Just "testkey"
  , Cfg.indexerUsername = Nothing
  , Cfg.indexerPassword = Nothing
  , Cfg.indexerEnabled = True
  , Cfg.indexerPriority = 10
  , Cfg.indexerCategories = [3000, 3010]
  , Cfg.indexerNormalizeQuery = False
  , Cfg.indexerResponseFormat = "json"
  }

-- | Transport that answers the indexer search after @micros@ of latency.
delayedIndexerTransport :: Int -> HttpTransport
delayedIndexerTransport micros =
  let base = mockTransport [ respond (methodIs "GET" .&. hostIs "mock.indexer") (textResponse newznabJson) ]
  in HttpTransport $ \req -> threadDelay micros >> runTransport base req

-- Handler wiring ------------------------------------------------------------

data CatalogHandlers = CatalogHandlers
  { chStream :: Int64 -> Maybe Text -> Handler (SourceIO ReleaseStreamEvent)
  , chCached :: Int64 -> Handler AlbumReleasesResponse
  }

-- | Point the config's indexer list at the mock indexer.
enableMockIndexer :: TestEnv -> IO ()
enableMockIndexer env =
  STM.atomically $ STM.modifyTVar' (teConfigVar env) $ \c ->
    c { Cfg.indexers = (Cfg.indexers c) { Cfg.indexerList = [mockIndexer] } }

-- | Wire the catalog server to the test env and pull out the two handlers we
-- exercise (auth is disabled under 'defaultConfig', so no token is needed).
mkCatalogHandlers :: TestEnv -> IO CatalogHandlers
mkCatalogHandlers env = do
  hex <- generateJWTSecretString
  mSecret <- getJWTSecret ((Cfg.server Cfg.defaultConfig) { Cfg.serverJwtSecret = Just hex })
  jwtSecret <- maybe (assertFailure "getJWTSecret returned Nothing") pure mSecret
  tm <- newTaskManager (teEventBus env) (teLogEnv env) (teClock env)
  let registry = ServiceRegistry
        { srMBClientEnv = teMBClientEnv env
        , srHttpClient = teHttpClient env
        , srConfigVar = teConfigVar env
        , srDownloadProgressMap = teProgressMap env
        , srClock = teClock env
        }
      server = catalogServer (teLogEnv env) (teEventBus env) (Cfg.server Cfg.defaultConfig)
                 jwtSecret registry tm (tePool env) (teCacheDir env) (teConfigVar env)
      _taskH :<|> _queryH :<|> _getArtistsH :<|> _getArtistH :<|> _createArtistH
        :<|> _updateArtistH :<|> _deleteArtistH :<|> _albumOverviewH :<|> _createAlbumH
        :<|> _updateAlbumH :<|> _deleteAlbumH :<|> cachedH :<|> streamH
        :<|> _getTracksH :<|> _getAlbumH :<|> _bulkH = server
  pure CatalogHandlers { chStream = streamH, chCached = cachedH }

-- | Drain a SourceIO into the list of events it yields.
collectSource :: SourceIO a -> IO [a]
collectSource (SourceT run) = run (go [])
  where
    go acc step = case step of
      Stop      -> pure (reverse acc)
      Skip s    -> go acc s
      Yield x s -> go (x : acc) s
      Effect eff -> eff >>= go acc
      Error e   -> assertFailure ("release stream errored: " <> e)

isDone :: ReleaseStreamEvent -> Bool
isDone (SearchDone _) = True
isDone _ = False

-- Tests ---------------------------------------------------------------------

testStreamsReleasesBeforeDone :: IO ()
testStreamsReleasesBeforeDone =
  withTestEnvTransport (delayedIndexerTransport 50000) $ \env -> do
    albumId <- withConnection (tePool env) $ \conn ->
      seedCatalogAlbum conn "Test Artist" "Test Album"
    enableMockIndexer env
    handlers <- mkCatalogHandlers env

    events <- collectSource =<< runOk (chStream handlers albumId Nothing)

    let releaseCount = length [() | ReleaseFound _ _ <- events]
    -- The core regression check: the mocked release must actually reach the
    -- consumer. The old fire-and-forget code ended the stream at SearchDone
    -- before the (delayed) release arrived, yielding zero releases here.
    assertEqual "one mocked release delivered to the stream" 1 releaseCount
    assertBool "stream terminates with SearchDone" (any isDone events)
    -- eventLoop stops at SearchDone, so nothing should follow it.
    let afterDone = drop 1 (dropWhile (not . isDone) events)
    assertBool "no events emitted after SearchDone" (null afterDone)

testStreamingPopulatesCache :: IO ()
testStreamingPopulatesCache =
  withTestEnvTransport (delayedIndexerTransport 0) $ \env -> do
    albumId <- withConnection (tePool env) $ \conn ->
      seedCatalogAlbum conn "Cache Artist" "Cache Album"
    enableMockIndexer env
    handlers <- mkCatalogHandlers env

    -- Run a streaming search to completion; it caches releases as a side effect.
    _ <- collectSource =<< runOk (chStream handlers albumId Nothing)

    -- The cached-releases endpoint should now return the release with no search.
    resp <- runOk (chCached handlers albumId)
    assertEqual "cached endpoint returns the streamed release"
      1 (length (albumReleasesReleases resp))
