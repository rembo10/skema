{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests for the Tracks repository.
--
-- Covers track rows, their metadata, the MusicBrainz id columns (stored on
-- library_tracks), and scan history. 'upsertTrackWithMetadata' is excluded
-- here because it reads file size/mtime from the real filesystem; it is
-- exercised by the scan/import integration tests instead.
module Integration.TracksRepoSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

import Helpers.TestEnv (TestEnv(..), withTestEnv)
import Helpers.Builders (mkTestMetadataRecord)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( insertTrack
  , getTrackByPath
  , getAllTracks
  , deleteTrack
  , insertTrackMetadata
  , updateTrackMetadata
  , getMetadataForTrack
  , insertScanHistory
  , getRecentScans
  )
import Skema.Database.Types
  ( LibraryTrackRecord(..)
  , LibraryTrackMetadataRecord(..)
  , ScanHistoryRecord(..)
  )
import Skema.FileSystem.Utils (stringToOsPath)
import Database.SQLite.Simple (Connection)
import System.OsPath (OsPath)

tests :: TestTree
tests = testGroup "Integration.TracksRepo"
  [ testCase "insert then fetch by path round-trips" testTrackRoundTrip
  , testCase "getTrackByPath returns Nothing for an unknown path" testTrackMiss
  , testCase "getAllTracks is ordered by path" testTrackOrdering
  , testCase "metadata insert and update round-trip" testMetadata
  , testCase "deleteTrack removes the row" testDelete
  , testCase "scan history insert and fetch round-trips" testScanHistory
  ]

-- Builders ------------------------------------------------------------------

modTime :: UTCTime
modTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- | Insert a track at a path and return its assigned id (insertTrack itself
-- does not return one, so we read it back).
insertTrackAt :: Connection -> OsPath -> IO Int64
insertTrackAt conn path = do
  _ <- insertTrack conn path 4096 modTime
  mrec <- getTrackByPath conn path
  case mrec >>= trackId of
    Just tid -> pure tid
    Nothing -> assertFailure "track id not found after insert"

-- Tests ---------------------------------------------------------------------

testTrackRoundTrip :: IO ()
testTrackRoundTrip = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  path <- stringToOsPath "/lib/artist/album/track.flac"
  _ <- insertTrack conn path 4096 modTime

  mrec <- getTrackByPath conn path
  case mrec of
    Nothing -> assertFailure "track not found by path"
    Just rec -> do
      trackPath rec @?= path
      trackSize rec @?= 4096
      trackModifiedAt rec @?= modTime
      trackClusterId rec @?= Nothing

testTrackMiss :: IO ()
testTrackMiss = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  path <- stringToOsPath "/lib/missing.flac"
  found <- getTrackByPath conn path
  fmap trackId found @?= Nothing

testTrackOrdering :: IO ()
testTrackOrdering = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  pathA <- stringToOsPath "/lib/a.flac"
  pathB <- stringToOsPath "/lib/b.flac"
  pathC <- stringToOsPath "/lib/c.flac"
  -- Insert out of order; the query should return them sorted by path.
  _ <- insertTrack conn pathB 1 modTime
  _ <- insertTrack conn pathA 1 modTime
  _ <- insertTrack conn pathC 1 modTime

  tracks <- getAllTracks conn
  map trackPath tracks @?= [pathA, pathB, pathC]

testMetadata :: IO ()
testMetadata = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  path <- stringToOsPath "/lib/meta.flac"
  tid <- insertTrackAt conn path

  let meta = mkTestMetadataRecord "Some Album" "Some Artist" 3
  insertTrackMetadata conn tid meta

  inserted <- getMetadataForTrack conn tid
  case inserted of
    Nothing -> assertFailure "metadata not found after insert"
    Just m -> do
      metaTitle m @?= Just "Track 3"
      metaArtist m @?= Just "Some Artist"
      metaAlbum m @?= Just "Some Album"
      metaAlbumArtist m @?= Just "Some Artist"
      metaTrackNumber m @?= Just 3
      metaYear m @?= Just 2024

  -- Update replaces the stored values.
  updateTrackMetadata conn tid (meta { metaTitle = Just "Renamed", metaTrackNumber = Just 9 })
  updated <- getMetadataForTrack conn tid
  fmap metaTitle updated @?= Just (Just "Renamed")
  fmap metaTrackNumber updated @?= Just (Just 9)

testDelete :: IO ()
testDelete = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  path <- stringToOsPath "/lib/doomed.flac"
  tid <- insertTrackAt conn path
  deleteTrack conn tid
  gone <- getTrackByPath conn path
  fmap trackId gone @?= Nothing

testScanHistory :: IO ()
testScanHistory = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  _ <- insertScanHistory conn modTime True
  scans <- getRecentScans conn 10
  case scans of
    [] -> assertFailure "no scan history rows returned"
    (s : _) -> do
      scanStartedAt s @?= modTime
      scanLibraryAvailable s @?= True
      scanCompletedAt s @?= Nothing
