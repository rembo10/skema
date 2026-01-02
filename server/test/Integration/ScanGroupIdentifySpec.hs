{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Integration test for the complete scan → group → identify workflow.
--
-- This test verifies the end-to-end behavior of:
-- 1. Scanning a library for audio files
-- 2. Grouping files into album clusters
-- 3. (Optionally) Identifying albums with MusicBrainz
module Integration.ScanGroupIdentifySpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Helpers.TestEnv
import Helpers.EventAssertions
import Helpers.TempLibrary
import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
import Skema.Database.Types (ClusterRecord(..), LibraryTrackMetadataRecord(..))
import Skema.Events.Bus (publishAndLog, subscribe)
import Skema.Events.Types
import Skema.Services.Scanner (startScannerService)
import Skema.Services.Grouper (startGrouperService)
import Skema.Services.Dependencies
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async (cancel)
import Data.List (nub)

tests :: TestTree
tests = testGroup "Integration.ScanGroupIdentify"
  [ testCase "scans library and groups files into albums" testScanAndGroup
  , testCase "handles multiple albums correctly" testMultipleAlbums
  , testCase "handles compilation albums with different artists" testCompilationAlbum
  ]

-- | Test basic scan and group workflow.
--
-- Creates a temp library with one album, scans it, and verifies
-- that files are correctly grouped into an album cluster.
testScanAndGroup :: IO ()
testScanAndGroup = withTestEnv $ \env -> do
  -- Create a test library with one album
  let album = mkSimpleAlbum "Test Artist" "Test Album" 10
  withTempLibraryStructure [album] $ \libPath -> do
    -- Subscribe to events before starting services
    eventChan <- STM.atomically $ subscribe (teEventBus env)

    -- Start scanner and grouper services
    scannerAsync <- startScannerService $ ScannerDeps
      { scanEventBus = teEventBus env
      , scanLogEnv = teLogEnv env
      , scanDbPool = tePool env
      }

    grouperAsync <- startGrouperService $ GrouperDeps
      { groupEventBus = teEventBus env
      , groupLogEnv = teLogEnv env
      , groupDbPool = tePool env
      , groupConfigVar = teConfigVar env
      }

    -- Trigger library scan
    publishAndLog (teEventBus env) (teLogEnv env) "test" $
      LibraryScanRequested (toText libPath)

    -- Wait for scan completion
    scanComplete <- waitForEventWithTimeout eventChan isMetadataReadComplete (10 * 1000000)
    assertBool "Scan should complete" (isJust scanComplete)

    -- Wait for grouping completion
    groupComplete <- waitForEventWithTimeout eventChan isClustersGenerated (5 * 1000000)
    assertBool "Grouping should complete" (isJust groupComplete)

    -- Verify database state
    clusters <- withConnection (tePool env) getAllClusters
    length clusters @?= 1

    case clusters of
      [] -> assertFailure "Expected at least one cluster"
      (cluster:_) -> case clusterId cluster of
        Nothing -> assertFailure "Cluster should have an ID"
        Just cid -> do
          -- Verify cluster has correct number of tracks
          maybeClusterData <- withConnection (tePool env) $ \conn ->
            getClusterWithTracks conn cid
          case maybeClusterData of
            Nothing -> assertFailure "Cluster data not found"
            Just (_clusterInfo, tracks) -> do
              length tracks @?= 10

              -- Verify metadata
              case tracks of
                [] -> assertFailure "Expected at least one track"
                ((_,_, firstMeta, _, _):_) -> do
                  metaAlbumArtist firstMeta @?= Just "Test Artist"
                  metaAlbum firstMeta @?= Just "Test Album"

    -- Cleanup: stop services
    cancel scannerAsync
    cancel grouperAsync

-- | Test scanning multiple albums.
--
-- Verifies that the system correctly identifies and groups
-- multiple different albums in the same scan.
testMultipleAlbums :: IO ()
testMultipleAlbums = withTestEnv $ \env -> do
  -- Create library with 3 different albums
  let albums =
        [ mkSimpleAlbum "Artist A" "Album 1" 5
        , mkSimpleAlbum "Artist B" "Album 2" 8
        , mkSimpleAlbum "Artist A" "Album 3" 6
        ]

  withTempLibraryStructure albums $ \libPath -> do
    eventChan <- STM.atomically $ subscribe (teEventBus env)

    -- Start services
    scannerAsync <- startScannerService $ ScannerDeps
      { scanEventBus = teEventBus env
      , scanLogEnv = teLogEnv env
      , scanDbPool = tePool env
      }

    grouperAsync <- startGrouperService $ GrouperDeps
      { groupEventBus = teEventBus env
      , groupLogEnv = teLogEnv env
      , groupDbPool = tePool env
      , groupConfigVar = teConfigVar env
      }

    -- Trigger scan
    publishAndLog (teEventBus env) (teLogEnv env) "test" $
      LibraryScanRequested (toText libPath)

    -- Wait for scan completion
    _ <- waitForEventWithTimeout eventChan isMetadataReadComplete (10 * 1000000)

    -- Wait for grouping to complete (at least one cluster generated)
    -- Note: Grouper creates all 3 clusters in a batch, so we only need to wait for one event
    _ <- waitForEventWithTimeout eventChan isClustersGenerated (5 * 1000000)

    -- Verify 3 clusters were created in database
    clusters <- withConnection (tePool env) getAllClusters
    length clusters @?= 3

    -- Verify track counts
    trackCounts <- forM clusters $ \cluster ->
      case clusterId cluster of
        Nothing -> pure 0
        Just cid -> do
          maybeData <- withConnection (tePool env) $ \conn ->
            getClusterWithTracks conn cid
          case maybeData of
            Nothing -> pure 0
            Just (_info, tracks) -> pure (length tracks)

    sort trackCounts @?= [5, 6, 8]

    -- Cleanup
    cancel scannerAsync
    cancel grouperAsync

-- | Test compilation album handling.
--
-- Verifies that albums with different track artists but same
-- album artist are grouped correctly.
testCompilationAlbum :: IO ()
testCompilationAlbum = withTestEnv $ \env -> do
  -- Create a compilation album with different track artists
  let compilation = AlbumSpec
        { albumArtist = "Various Artists"
        , albumTitle = "Best of 2024"
        , albumTracks =
            [ TrackSpec 1 "Song One" (Just "Artist A") (Just 200.0)
            , TrackSpec 2 "Song Two" (Just "Artist B") (Just 180.0)
            , TrackSpec 3 "Song Three" (Just "Artist C") (Just 220.0)
            , TrackSpec 4 "Song Four" (Just "Artist A") (Just 190.0)
            ]
        }

  withTempLibraryStructure [compilation] $ \libPath -> do
    eventChan <- STM.atomically $ subscribe (teEventBus env)

    -- Start services
    scannerAsync <- startScannerService $ ScannerDeps
      { scanEventBus = teEventBus env
      , scanLogEnv = teLogEnv env
      , scanDbPool = tePool env
      }

    grouperAsync <- startGrouperService $ GrouperDeps
      { groupEventBus = teEventBus env
      , groupLogEnv = teLogEnv env
      , groupDbPool = tePool env
      , groupConfigVar = teConfigVar env
      }

    -- Trigger scan
    publishAndLog (teEventBus env) (teLogEnv env) "test" $
      LibraryScanRequested (toText libPath)

    -- Wait for completion
    _ <- waitForEventWithTimeout eventChan isMetadataReadComplete (10 * 1000000)
    _ <- waitForEventWithTimeout eventChan isClustersGenerated (5 * 1000000)

    -- Should create ONE cluster (grouped by album artist)
    clusters <- withConnection (tePool env) getAllClusters
    length clusters @?= 1

    -- Verify all 4 tracks in the cluster
    case clusters of
      [] -> assertFailure "Expected at least one cluster"
      (cluster:_) -> case clusterId cluster of
        Nothing -> assertFailure "Cluster should have an ID"
        Just cid -> do
          maybeData <- withConnection (tePool env) $ \conn ->
            getClusterWithTracks conn cid
          case maybeData of
            Nothing -> assertFailure "Cluster data not found"
            Just (_info, tracks) -> do
              length tracks @?= 4

              -- All should have same album artist
              let albumArtists = mapMaybe (\(_,_,meta,_,_) -> metaAlbumArtist meta) tracks
              all (== "Various Artists") albumArtists @?= True

              -- But different track artists
              let trackArtists = mapMaybe (\(_,_,meta,_,_) -> metaArtist meta) tracks
              length (nub trackArtists) @?= 3  -- Three different artists

    -- Cleanup
    cancel scannerAsync
    cancel grouperAsync

-- TODO: Add test for identification workflow
-- This would require either:
-- 1. Mocking MusicBrainz HTTP responses (complex)
-- 2. Using pre-recorded responses (better for integration tests)
-- 3. Testing identification logic separately with mock data
--
-- Example outline:
-- testIdentification :: IO ()
-- testIdentification = withTestEnvAndMocks $ \env mock -> do
--   -- Add mock MusicBrainz responses
--   addMockRelease mock "release-mbid" testRelease
--
--   -- Start identifier service
--   -- Trigger identification
--   -- Wait for AlbumIdentified event
--   -- Verify database updated with MB IDs
