{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests for the Downloads repository.
--
-- These exercise the SQL directly against a temporary database. They guard
-- two things that drift silently: the alignment between
-- 'downloadSelectColumns' and the 'DownloadRecord' row parser (a column
-- reorder would corrupt every field), and the status-string filters in the
-- active/failed queries (which must match the text the rest of the code
-- writes).
module Integration.DownloadsRepoSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

import Helpers.TestEnv (TestEnv(..), withTestEnv)
import Helpers.Builders (seedCatalogAlbum)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( insertDownload
  , getDownloadById
  , hasActiveDownloadForAlbum
  , getFailedDownloadUrlsForAlbum
  , getFailedDownloadCountForAlbum
  )
import Skema.Database.Repository.Downloads (deleteDownload, getAllDownloads)
import Skema.Database.Types (DownloadRecord(..), DownloadStatus(..))
import Database.SQLite.Simple (Connection)

tests :: TestTree
tests = testGroup "Integration.DownloadsRepo"
  [ testCase "insert then fetch preserves all fields" testRoundTrip
  , testCase "hasActiveDownloadForAlbum ignores failed and cancelled" testHasActive
  , testCase "getFailedDownloadUrlsForAlbum returns only failed urls" testFailedUrls
  , testCase "getFailedDownloadCountForAlbum counts only failed" testFailedCount
  , testCase "getAllDownloads orders by queued_at descending" testOrdering
  , testCase "deleteDownload removes the row" testDelete
  ]

-- Builders ------------------------------------------------------------------

day :: Integer -> Int -> Int -> UTCTime
day y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

-- | Insert a download for an album with a given status, url and queued time,
-- filling the remaining columns with simple defaults.
insertFor :: Connection -> Int64 -> Text -> Text -> UTCTime -> IO Int64
insertFor conn albumId status url queuedAt =
  insertDownload conn
    (Just albumId)
    "test-indexer"
    url
    "slskd"
    Nothing
    status
    Nothing
    "Some Album"
    Nothing
    Nothing
    Nothing
    Nothing
    0.0
    Nothing
    queuedAt

-- Tests ---------------------------------------------------------------------

testRoundTrip :: IO ()
testRoundTrip = withTestEnv $ \env -> do
  let queuedAt = day 2025 6 1
  (albumId, dlId, mrec) <- withConnection (tePool env) $ \conn -> do
    albumId <- seedCatalogAlbum conn "Round Trip Artist" "Round Trip Album"
    dlId <- insertDownload conn
      (Just albumId)
      "indexer-x"
      "http://example/dl"
      "sabnzbd"
      (Just "client-7")
      "downloading"
      (Just "/downloads/rt")
      "Round Trip Album"
      (Just 123456)
      (Just "Lossless")
      (Just "NZB")
      (Just 9)
      0.0
      Nothing
      queuedAt
    mrec <- getDownloadById conn dlId
    pure (albumId, dlId, mrec)

  case mrec of
    Nothing -> assertFailure "inserted download was not found by id"
    Just rec -> do
      downloadId rec @?= Just dlId
      downloadCatalogAlbumId rec @?= albumId
      downloadIndexerName rec @?= "indexer-x"
      downloadUrl rec @?= "http://example/dl"
      downloadClient rec @?= Just "sabnzbd"
      downloadClientId rec @?= Just "client-7"
      downloadStatus rec @?= DownloadDownloading
      downloadPath rec @?= Just "/downloads/rt"
      downloadTitle rec @?= "Round Trip Album"
      downloadSizeBytes rec @?= Just 123456
      downloadQuality rec @?= Just "Lossless"
      downloadFormat rec @?= Just "NZB"
      downloadSeeders rec @?= Just 9
      downloadProgress rec @?= 0.0
      downloadErrorMessage rec @?= Nothing
      downloadQueuedAt rec @?= Just queuedAt

testHasActive :: IO ()
testHasActive = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  albumId <- seedCatalogAlbum conn "Active Artist" "Active Album"
  otherId <- seedCatalogAlbum conn "Other Artist" "Other Album"

  -- An album with only failed/cancelled downloads is not active.
  _ <- insertFor conn albumId "failed" "u1" (day 2025 1 1)
  _ <- insertFor conn albumId "cancelled" "u2" (day 2025 1 2)
  inactive <- hasActiveDownloadForAlbum conn albumId
  inactive @?= False

  -- Adding a downloading entry makes it active.
  _ <- insertFor conn albumId "downloading" "u3" (day 2025 1 3)
  active <- hasActiveDownloadForAlbum conn albumId
  active @?= True

  -- An album with no downloads at all is not active.
  none <- hasActiveDownloadForAlbum conn otherId
  none @?= False

testFailedUrls :: IO ()
testFailedUrls = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  albumId <- seedCatalogAlbum conn "Failed Artist" "Failed Album"
  _ <- insertFor conn albumId "failed" "failed-1" (day 2025 1 1)
  _ <- insertFor conn albumId "failed" "failed-2" (day 2025 1 2)
  _ <- insertFor conn albumId "downloading" "active-1" (day 2025 1 3)
  _ <- insertFor conn albumId "cancelled" "cancelled-1" (day 2025 1 4)

  urls <- getFailedDownloadUrlsForAlbum conn albumId
  sortOn id urls @?= ["failed-1", "failed-2"]

testFailedCount :: IO ()
testFailedCount = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  albumId <- seedCatalogAlbum conn "Count Artist" "Count Album"
  _ <- insertFor conn albumId "failed" "f1" (day 2025 1 1)
  _ <- insertFor conn albumId "failed" "f2" (day 2025 1 2)
  _ <- insertFor conn albumId "downloading" "a1" (day 2025 1 3)

  count <- getFailedDownloadCountForAlbum conn albumId
  count @?= 2

testOrdering :: IO ()
testOrdering = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  albumId <- seedCatalogAlbum conn "Order Artist" "Order Album"
  -- Insert out of chronological order; query should sort newest-first.
  _ <- insertFor conn albumId "downloading" "middle" (day 2025 3 1)
  _ <- insertFor conn albumId "downloading" "oldest" (day 2025 1 1)
  _ <- insertFor conn albumId "downloading" "newest" (day 2025 6 1)

  downloads <- getAllDownloads conn
  map downloadUrl downloads @?= ["newest", "middle", "oldest"]

testDelete :: IO ()
testDelete = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  albumId <- seedCatalogAlbum conn "Delete Artist" "Delete Album"
  dlId <- insertFor conn albumId "downloading" "to-delete" (day 2025 1 1)

  before <- getDownloadById conn dlId
  assertBool "download should exist before delete" (isJust before)

  deleteDownload conn dlId
  afterDelete <- getDownloadById conn dlId
  afterDelete @?= Nothing
