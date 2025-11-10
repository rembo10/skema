{-# LANGUAGE OverloadedStrings #-}

module Integration.GrouperSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Skema.Database.Connection
import Skema.Database.Migrations
import Skema.Database.Repository
import Skema.Database.Types
import Skema.Services.Grouper (handleMetadataReadComplete)
import Skema.Services.Dependencies (GrouperDeps(..))
import Skema.Services.Types
import Skema.Events.Bus
import Data.Time (getCurrentTime)
import System.IO.Temp (withSystemTempFile)
import System.OsPath (encodeUtf)
import qualified System.Timeout as Timeout
import Control.Exception (bracket)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import Katip

tests :: TestTree
tests = testGroup "Integration.Grouper"
  [ testCase "handles many file groups without connection pool deadlock" $
      withTimeout $ testGrouperWithManyAlbums
  ]

-- | Wrap test with a timeout to catch deadlocks
withTimeout :: IO () -> IO ()
withTimeout action = do
  result <- Timeout.timeout (10 * 1000000) action  -- 10 second timeout
  case result of
    Nothing -> assertFailure "Test timed out - likely connection pool deadlock"
    Just () -> pure ()

-- | Test that the Grouper service can handle many albums without deadlocking
testGrouperWithManyAlbums :: IO ()
testGrouperWithManyAlbums = do
  withTestDb $ \ctx -> do
    let pool = scDbPool ctx
    let bus = scEventBus ctx
    let le = scLogEnv ctx
    let dummyConfig = scConfigVar ctx

    -- Insert 30 tracks across 3 albums (10 tracks each)
    -- This should be enough to trigger the deadlock if the bug exists
    now <- getCurrentTime
    withConnection pool $ \conn -> do
      -- Album 1: "Test Album 1" by "Test Artist 1"
      forM_ [1..10 :: Int] $ \trackNum -> do
        path <- encodeUtf $ "/test/album1/track" <> show trackNum <> ".flac"
        _ <- insertTrack conn path 1000000 now
        let metaRec = createTestMetadataRecord "Test Album 1" "Test Artist 1" trackNum
        maybeTrack <- getTrackByPath conn path
        case maybeTrack >>= trackId of
          Just tid -> insertTrackMetadata conn tid metaRec
          Nothing -> pure ()

      -- Album 2: "Test Album 2" by "Test Artist 2"
      forM_ [1..10 :: Int] $ \trackNum -> do
        path <- encodeUtf $ "/test/album2/track" <> show trackNum <> ".flac"
        _ <- insertTrack conn path 1000000 now
        let metaRec = createTestMetadataRecord "Test Album 2" "Test Artist 2" trackNum
        maybeTrack <- getTrackByPath conn path
        case maybeTrack >>= trackId of
          Just tid -> insertTrackMetadata conn tid metaRec
          Nothing -> pure ()

      -- Album 3: "Test Album 3" by "Test Artist 3"
      forM_ [1..10 :: Int] $ \trackNum -> do
        path <- encodeUtf $ "/test/album3/track" <> show trackNum <> ".flac"
        _ <- insertTrack conn path 1000000 now
        let metaRec = createTestMetadataRecord "Test Album 3" "Test Artist 3" trackNum
        maybeTrack <- getTrackByPath conn path
        case maybeTrack >>= trackId of
          Just tid -> insertTrackMetadata conn tid metaRec
          Nothing -> pure ()

    -- Call the grouper service (this should complete without hanging)
    let grouperDeps = GrouperDeps
          { groupEventBus = bus
          , groupLogEnv = le
          , groupDbPool = pool
          , groupConfigVar = dummyConfig
          }
    handleMetadataReadComplete grouperDeps 30

    -- Verify clusters were created
    clusters <- withConnection pool getAllClusters
    length clusters @?= 3

    -- Verify each cluster has the correct number of tracks
    clusterTracks <- withConnection pool $ \conn ->
      forM clusters $ \cluster -> do
        case clusterId cluster of
          Nothing -> pure []
          Just cid -> do
            result <- getClusterWithTracks conn cid
            case result of
              Nothing -> pure []
              Just (_, tracks) -> pure tracks

    let trackCounts = map length clusterTracks
    sort trackCounts @?= [10, 10, 10]

-- Test Helpers

-- | Create a test metadata record with minimal required fields
createTestMetadataRecord :: Text -> Text -> Int -> LibraryTrackMetadataRecord
createTestMetadataRecord album albumArtist trackNum = LibraryTrackMetadataRecord
  { metaTrackId = Nothing
  , metaFormat = Nothing
  , metaTitle = Just $ "Track " <> show trackNum
  , metaArtist = Just albumArtist
  , metaAlbum = Just album
  , metaAlbumArtist = Just albumArtist
  , metaTrackNumber = Just trackNum
  , metaTotalTracks = Nothing
  , metaDiscNumber = Just 1
  , metaTotalDiscs = Nothing
  , metaYear = Just 2024
  , metaDurationSeconds = Just 180.0
  , metaGenre = Nothing
  , metaDate = Nothing
  , metaPublisher = Nothing
  , metaComment = Nothing
  , metaBitsPerSample = Nothing
  , metaBarcode = Nothing
  , metaCatalogNumber = Nothing
  , metaLabel = Nothing
  , metaCountry = Nothing
  , metaReleaseStatus = Nothing
  , metaReleaseType = Nothing
  , metaMBRecordingId = Nothing
  , metaMBTrackId = Nothing
  , metaMBReleaseId = Nothing
  , metaMBReleaseGroupId = Nothing
  , metaMBArtistId = Nothing
  , metaMBAlbumArtistId = Nothing
  , metaMBWorkId = Nothing
  , metaMBDiscId = Nothing
  , metaAcoustidFingerprint = Nothing
  , metaAcoustidId = Nothing
  , metaCreatedAt = Nothing
  , metaUpdatedAt = Nothing
  }

-- | Create a temporary SQLite database for testing with a small connection pool
withTestDb :: (ServiceContext -> IO a) -> IO a
withTestDb action = do
  withSystemTempFile "skema-test.db" $ \tmpFile _handle -> do
    -- Create a minimal LogEnv for testing (only show errors and critical messages)
    handleScribe <- mkHandleScribe ColorIfTerminal stderr (permitItem ErrorS) V2
    let mkLogEnv = registerScribe "stderr" handleScribe defaultScribeSettings =<< initLogEnv "skema-test" "test"

    bracket mkLogEnv closeScribes $ \le -> do
      -- Create database config with SMALL pool size to expose deadlock
      let dbConfig = DatabaseConfig
            { dbPath = tmpFile
            , dbPoolSize = 2  -- Small pool to trigger deadlock faster
            }

      -- Create connection pool and run migrations
      bracket (createConnectionPool le dbConfig) destroyConnectionPool $ \pool -> do
        runMigrations le pool

        -- Create event bus
        bus <- newEventBus

        -- Create a dummy config TVar (not used in this test)
        dummyConfig <- STM.newTVarIO $ error "Config not needed for this test"

        -- Create empty progress map (not used in this test)
        progressMap <- STM.newTVarIO Map.empty

        -- Create service context
        let ctx = ServiceContext
              { scEventBus = bus
              , scDbPool = pool
              , scConfigVar = dummyConfig
              , scLogEnv = le
              , scHttpClient = error "HTTP client not needed for this test"
              , scMBClientEnv = error "MB client not needed for this test"
              , scCacheDir = "/tmp"
              , scDownloadProgressMap = progressMap
              }

        action ctx
