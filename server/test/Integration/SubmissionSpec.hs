{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Integration tests for 'submitDownload'.
--
-- These exercise the slskd submission path end-to-end against a real
-- (temporary) database and a mocked HTTP transport. They double as the
-- reference example for testing a service action under a controlled clock:
-- 'dscClock' is a 'fixedClock', so the @queued_at@ timestamp written to
-- the database is deterministic and can be asserted exactly.
module Integration.SubmissionSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Control.Concurrent.STM as STM

import Helpers.TestEnv (TestEnv(..), withTestEnvMock)
import Helpers.MockHttp (RequestMatcher, respond, methodIs, pathIs, textResponse, (.&.))
import Helpers.EventAssertions (waitForEventWithTimeout)

import Skema.Clock (fixedClock)
import Skema.Database.Connection (withConnection)
import Skema.Database.Repository (getDownloadById, upsertCatalogArtist, upsertCatalogAlbum)
import Skema.Database.Types (DownloadRecord(..), DownloadStatus(..))
import Skema.Events.Bus (subscribe)
import Skema.Events.Types (Event(..))
import qualified Skema.Domain.Quality as Quality
import Skema.Config.Types (Config(..), DownloadConfig(..), SlskdConfig(..), defaultConfig)
import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..), SlskdFile(..))
import Skema.Services.Download.Submission (submitDownload, DownloadSubmissionContext(..))

tests :: TestTree
tests = testGroup "Integration.Submission"
  [ testCase "slskd submission persists a download stamped with the injected clock" testSlskdSubmitStampsClock
  , testCase "submitDownload skips when an active download already exists" testDuplicatePrevention
  ]

-- | A frozen instant used as the injected "now".
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 6 1) (secondsToDiffTime 0)

-- | Mock the slskd "queue downloads" endpoint for user "testuser".
-- The body is ignored by the client on success, so any 2xx response works.
slskdQueueMatcher :: RequestMatcher
slskdQueueMatcher =
  respond (methodIs "POST" .&. pathIs "/api/v0/transfers/downloads/testuser")
          (textResponse "ok")

-- | A slskd download config pointed at the mocked host.
testSlskdConfig :: SlskdConfig
testSlskdConfig = SlskdConfig
  { slskdUrl = "http://localhost:5030"
  , slskdApiKey = "test-api-key"
  , slskdEnabled = True
  , slskdDownloadDirectory = "/downloads/slskd"
  }

-- | A release describing a single-file slskd download.
testRelease :: ReleaseInfo
testRelease = ReleaseInfo
  { riTitle = "Test Album"
  , riGuid = Nothing
  , riDownloadUrl = ""
  , riInfoUrl = Nothing
  , riSize = Just 5242880
  , riPublishDate = Nothing
  , riCategory = Nothing
  , riSeeders = Nothing
  , riPeers = Nothing
  , riGrabs = Nothing
  , riDownloadType = Slskd
  , riQuality = Quality.Lossless
  , riSlskdUsername = Just "testuser"
  , riSlskdFiles = Just
      [ SlskdFile
          { sfFilename = "music/test.flac"
          , sfSize = 5242880
          , sfBitRate = Nothing
          , sfSampleRate = Nothing
          , sfBitDepth = Nothing
          , sfLength = Nothing
          , sfIsLocked = False
          }
      ]
  }

-- | Build a submission context for the test env with a frozen clock.
mkContext :: TestEnv -> DownloadSubmissionContext
mkContext env = DownloadSubmissionContext
  { dscEventBus = teEventBus env
  , dscLogEnv = teLogEnv env
  , dscDbPool = tePool env
  , dscHttpClient = teHttpClient env
  , dscDownloadConfig = (download defaultConfig) { downloadSlskdClient = Just testSlskdConfig }
  , dscIndexerName = "slskd"
  , dscClock = fixedClock fixedTime
  }

-- | Insert an artist + album so the download has a valid catalog_album_id
-- (the downloads table has a NOT NULL FK to catalog_albums).
seedCatalogAlbum :: TestEnv -> IO Int64
seedCatalogAlbum env = withConnection (tePool env) $ \conn -> do
  artistId <- upsertCatalogArtist conn "artist-mbid" "Test Artist"
                Nothing Nothing Nothing True Nothing Nothing Nothing
  upsertCatalogAlbum conn "rg-mbid" "Test Album" artistId
    "artist-mbid" "Test Artist" Nothing Nothing

isDownloadStarted :: Event -> Bool
isDownloadStarted = \case
  DownloadStarted{} -> True
  _ -> False

testSlskdSubmitStampsClock :: IO ()
testSlskdSubmitStampsClock = withTestEnvMock [slskdQueueMatcher] $ \env -> do
  albumId <- seedCatalogAlbum env
  eventChan <- STM.atomically $ subscribe (teEventBus env)

  result <- submitDownload (mkContext env) testRelease albumId

  case result of
    Nothing -> assertFailure "expected submitDownload to return Just downloadId"
    Just dlId -> do
      -- The DownloadStarted event fired.
      started <- waitForEventWithTimeout eventChan isDownloadStarted (1 * 1000000)
      assertBool "DownloadStarted event should be emitted" (isJust started)

      -- The persisted row carries the injected clock's time and is active.
      mrec <- withConnection (tePool env) $ \conn -> getDownloadById conn dlId
      case mrec of
        Nothing -> assertFailure "download row was not persisted"
        Just rec -> do
          downloadQueuedAt rec @?= Just fixedTime
          downloadStatus rec @?= DownloadDownloading
          downloadCatalogAlbumId rec @?= albumId

testDuplicatePrevention :: IO ()
testDuplicatePrevention = withTestEnvMock [slskdQueueMatcher] $ \env -> do
  albumId <- seedCatalogAlbum env
  let ctx = mkContext env

  -- First submission succeeds and leaves an active ("downloading") download.
  firstResult <- submitDownload ctx testRelease albumId
  assertBool "first submission should succeed" (isJust firstResult)

  -- Second submission for the same album is skipped (no duplicate row).
  secondResult <- submitDownload ctx testRelease albumId
  secondResult @?= Nothing
