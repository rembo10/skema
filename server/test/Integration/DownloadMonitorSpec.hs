{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integration.DownloadMonitorSpec (spec) where

import Test.Hspec
import Database.SQLite.Simple
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)

import Skema.Database.Connection (newPool, withConnection)
import Skema.Database.Repository (insertDownload)
import Skema.Events.Bus (newEventBus)
import Skema.Services.Download (checkAndUpdateDownloads, DownloadClientInstance(..))
import Skema.DownloadClient.SABnzbd (createSABnzbdClient)
import Skema.HTTP.Client (newHttpClient, defaultHttpConfig, defaultUserAgentData)
import qualified Skema.DownloadClient.Types as DC
import Katip

-- Mock SABnzbd client that returns fake progress
mockSABnzbdClient :: SABnzbdClient
mockSABnzbdClient = undefined  -- TODO: Create mock

spec :: Spec
spec = describe "Download Monitor Integration" $ do

  it "should update progress map when download is active" $ do
    -- Setup: Create test database, insert a download
    pool <- newPool ":memory:"
    now <- getCurrentTime

    downloadId <- withConnection pool $ \conn -> do
      -- Create downloads table
      execute_ conn "CREATE TABLE downloads (\
        \id INTEGER PRIMARY KEY AUTOINCREMENT, \
        \catalog_album_id INTEGER, \
        \indexer_name TEXT NOT NULL, \
        \download_url TEXT NOT NULL, \
        \download_client TEXT, \
        \download_client_id TEXT, \
        \status TEXT NOT NULL DEFAULT 'queued', \
        \download_path TEXT, \
        \title TEXT NOT NULL, \
        \size_bytes INTEGER, \
        \quality TEXT, \
        \format TEXT, \
        \seeders INTEGER, \
        \progress REAL DEFAULT 0.0, \
        \error_message TEXT, \
        \queued_at TIMESTAMP NOT NULL, \
        \started_at TIMESTAMP, \
        \completed_at TIMESTAMP, \
        \imported_at TIMESTAMP, \
        \updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"

      -- Insert test download
      insertDownload conn
        (Just 1)  -- catalog_album_id
        "test-indexer"
        "http://example.com/test.nzb"
        "sabnzbd"  -- IMPORTANT: Non-empty client name
        (Just "test_nzo_id")
        "downloading"
        Nothing
        "Test Album"
        (Just 100000000)
        Nothing
        (Just "NZB")
        Nothing
        0.0
        Nothing
        now

    -- Setup: Create mock client that returns 50% progress
    let mockClient = SABInstance mockSABnzbdClient

    -- Setup: Create progress map and event bus
    progressMap <- newTVarIO Map.empty
    bus <- newEventBus
    le <- initLogEnv "test" "test"
    httpClient <- newHttpClient le defaultHttpConfig defaultUserAgentData

    -- Action: Run monitor check
    checkAndUpdateDownloads le bus pool progressMap "sabnzbd" mockClient

    -- Assert: Progress map should be updated
    progressData <- readTVarIO progressMap
    Map.lookup downloadId progressData `shouldSatisfy` \case
      Just (progress, _status) -> progress == 0.5
      Nothing -> False

  it "should emit DownloadProgress events" $ pending

  it "should emit DownloadCompleted when status changes" $ pending

  it "should not find downloads with mismatched client names" $ do
    -- This test verifies the bug we found
    pool <- newPool ":memory:"
    now <- getCurrentTime

    withConnection pool $ \conn -> do
      execute_ conn "CREATE TABLE downloads (\
        \id INTEGER PRIMARY KEY, \
        \catalog_album_id INTEGER, \
        \indexer_name TEXT NOT NULL, \
        \download_url TEXT NOT NULL, \
        \download_client TEXT, \
        \download_client_id TEXT, \
        \status TEXT NOT NULL, \
        \download_path TEXT, \
        \title TEXT NOT NULL, \
        \size_bytes INTEGER, \
        \quality TEXT, \
        \format TEXT, \
        \seeders INTEGER, \
        \progress REAL, \
        \error_message TEXT, \
        \queued_at TIMESTAMP NOT NULL, \
        \started_at TIMESTAMP, \
        \completed_at TIMESTAMP, \
        \imported_at TIMESTAMP, \
        \updated_at TIMESTAMP)"

      -- Insert download with empty client name (the bug)
      insertDownload conn
        (Just 1)
        "test-indexer"
        "http://example.com/test.nzb"
        ""  -- EMPTY client name
        (Just "test_nzo_id")
        "downloading"
        Nothing
        "Test Album"
        Nothing
        Nothing
        Nothing
        Nothing
        0.0
        Nothing
        now

    -- Query with non-empty client name (what monitor does)
    rows <- withConnection pool $ \conn ->
      query conn
        "SELECT id FROM downloads WHERE download_client = ? AND status = ?"
        ("sabnzbd" :: String, "downloading" :: String) :: IO [Only Int]

    -- Should find NO downloads (demonstrates the bug)
    length rows `shouldBe` 0
