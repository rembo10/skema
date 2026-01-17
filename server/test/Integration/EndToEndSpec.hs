{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | End-to-end integration test for the complete workflow.
--
-- This test verifies the entire flow from scanning a library through
-- to importing downloaded albums:
-- 1. Scan library → discover audio files
-- 2. Group files into album clusters
-- 3. Identify albums with MusicBrainz (requires HTTP mocking - TODO)
-- 4. Add catalog artists for identified artists
-- 5. Fetch artist discography
-- 6. Mark albums as wanted based on acquisition rules
-- 7. Send wanted albums to downloader
-- 8. Import downloaded albums back into library
--
-- NOTE: Currently tests scan → group workflow. Identification and beyond
-- require HTTP mocking infrastructure to be completed (see Helpers/MockHTTP.hs).
module Integration.EndToEndSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Helpers.TestEnv
import Helpers.EventAssertions
import Helpers.TempLibrary
import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
import Skema.Database.Types (ClusterRecord(..))
import Skema.Events.Bus (publishAndLog, subscribe)
import Skema.Events.Types
import Skema.Services.Scanner (startScannerService)
import Skema.Services.Grouper (startGrouperService)
import Skema.Services.Dependencies
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async (cancel)

tests :: TestTree
tests = testGroup "Integration.EndToEnd"
  [ testCase "end-to-end workflow: scan and group" testScanAndGroupWorkflow
  -- TODO: Add these tests when HTTP mocking is implemented
  -- , testCase "end-to-end workflow: identify albums" testIdentifyWorkflow
  -- , testCase "end-to-end workflow: add catalog artists" testCatalogArtistWorkflow
  -- , testCase "end-to-end workflow: fetch discography" testDiscographyWorkflow
  -- , testCase "end-to-end workflow: mark wanted albums" testWantedAlbumWorkflow
  -- , testCase "end-to-end workflow: download and import" testDownloadImportWorkflow
  ]

-- | Test the complete scan and group workflow.
--
-- This verifies:
-- - Files are discovered and scanned
-- - Metadata is read from FLAC files
-- - Files are correctly grouped into album clusters
-- - Database state is updated correctly
-- - Events are emitted in correct order
testScanAndGroupWorkflow :: IO ()
testScanAndGroupWorkflow = withTestEnv $ \env -> do
  -- Create a realistic test library with multiple artists and albums
  let albums =
        [ mkSimpleAlbum "Kendrick Lamar" "good kid, m.A.A.d city" 12
        , mkSimpleAlbum "Kendrick Lamar" "To Pimp a Butterfly" 16
        , mkSimpleAlbum "J. Cole" "2014 Forest Hills Drive" 13
        , AlbumSpec
            { albumArtist = "Various Artists"
            , albumTitle = "Grand Theft Auto V Soundtrack"
            , albumTracks =
                [ TrackSpec 1 "Welcome to Los Santos" (Just "Various Artists") (Just 180.0)
                , TrackSpec 2 "Hood Gone Love It" (Just "Jay Rock") (Just 220.0)
                , TrackSpec 3 "Sleepwalkin'" (Just "The Chain Gang of 1974") (Just 195.0)
                ]
            }
        ]

  withTempLibraryStructure albums $ \libPath -> do
    -- Subscribe to all events to verify workflow
    eventChan <- STM.atomically $ subscribe (teEventBus env)

    -- Start required services
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
      LibraryScanRequested (toText libPath) False

    -- Wait for scan to complete
    scanResult <- waitForEventWithTimeout eventChan isMetadataReadComplete (15 * 1000000)
    assertBool "Scan should complete" (isJust scanResult)

    -- Verify scan results
    case scanResult of
      Just (MetadataReadComplete{ filesProcessed = count }) -> do
        -- Should have scanned 44 files (12 + 16 + 13 + 3)
        count @?= 44
      _ -> assertFailure "Expected MetadataReadComplete event"

    -- Wait for grouping to complete
    groupResult <- waitForEventWithTimeout eventChan isClustersGenerated (10 * 1000000)
    assertBool "Grouping should complete" (isJust groupResult)

    -- Verify grouping results
    case groupResult of
      Just (ClustersGenerated{ totalGroups = groups, needsIdentification = needsId }) -> do
        -- Should create 4 clusters (4 albums)
        groups @?= 4
        -- All 4 should need identification
        needsId @?= 4
      _ -> assertFailure "Expected ClustersGenerated event"

    -- Verify database state
    clusters <- withConnection (tePool env) getAllClusters
    length clusters @?= 4

    -- Verify each cluster has correct number of tracks
    trackCounts <- forM clusters $ \cluster ->
      case clusterId cluster of
        Nothing -> pure 0
        Just cid -> do
          maybeData <- withConnection (tePool env) $ \conn ->
            getClusterWithTracks conn cid
          case maybeData of
            Nothing -> pure 0
            Just (_info, tracks) -> pure (length tracks)

    -- Should have [3, 12, 13, 16] tracks
    sort trackCounts @?= [3, 12, 13, 16]

    -- Verify cluster metadata
    let albumTitles = mapMaybe clusterAlbum clusters
    sort albumTitles @?= sort
      [ "2014 Forest Hills Drive"
      , "Grand Theft Auto V Soundtrack"
      , "To Pimp a Butterfly"
      , "good kid, m.A.A.d city"
      ]

    -- Verify artists
    let artists = mapMaybe clusterAlbumArtist clusters
    sort artists @?= sort
      [ "J. Cole"
      , "Kendrick Lamar"
      , "Kendrick Lamar"
      , "Various Artists"
      ]

    -- Cleanup
    cancel scannerAsync
    cancel grouperAsync

-- ============================================================================
-- TODO: Implement these tests when HTTP mocking is fully functional
-- ============================================================================
--
-- NOTE: The tests below are templates showing how to test the full workflow.
-- They require HTTP mocking to intercept MusicBrainz API calls.
--
-- To implement HTTP mocking, see Helpers/MockHTTP.hs and choose one of:
-- 1. Create a local test HTTP server with warp that returns mock responses
-- 2. Mock at the Network.HTTP.Client Manager level (complex)
-- 3. Mock at the Skema.MusicBrainz.Client level instead of HTTP.Client
-- 4. Use a library like http-client-mock or mockery
--
-- Once HTTP mocking is working:
-- 1. Uncomment these tests in the TestTree above
-- 2. Update imports as needed
-- 3. Run and verify
--
-- ============================================================================

-- | Test identification workflow: scan → group → identify
--
-- testIdentifyWorkflow :: IO ()
-- testIdentifyWorkflow = withTestEnvAndMocks $ \env mock -> do
--   -- Create test library with a known album
--   let album = mkSimpleAlbum "Kendrick Lamar" "good kid, m.A.A.d city" 12
--
--   withTempLibraryStructure [album] $ \libPath -> do
--     -- Subscribe to events
--     eventChan <- STM.atomically $ subscribe (teEventBus env)
--
--     -- Configure mock MusicBrainz responses
--     -- The identifier will search for releases matching the album metadata
--     let mockRelease = mkTestMBRelease
--           "a1b2c3d4-e5f6-7890-abcd-ef1234567890"  -- Release MBID
--           "good kid, m.A.A.d city"                -- Album title
--           "Kendrick Lamar"                        -- Artist name
--           "381086ea-f511-4aba-bdf9-71c753dc5077"  -- Artist MBID (Kendrick's real ID)
--           12                                       -- Track count
--
--     -- Mock the MusicBrainz release search endpoint
--     -- This is what the identifier service calls to find matching releases
--     addJSONResponse mock "musicbrainz.org" $ mkTestMBReleaseSearch [mockRelease]
--
--     -- Start all required services
--     scannerAsync <- startScannerService $ ScannerDeps
--       { scanEventBus = teEventBus env
--       , scanLogEnv = teLogEnv env
--       , scanDbPool = tePool env
--       }
--
--     grouperAsync <- startGrouperService $ GrouperDeps
--       { groupEventBus = teEventBus env
--       , groupLogEnv = teLogEnv env
--       , groupDbPool = tePool env
--       , groupConfigVar = teConfigVar env
--       }
--
--     identifierAsync <- startIdentifierService $ IdentifierDeps
--       { identEventBus = teEventBus env
--       , identLogEnv = teLogEnv env
--       , identDbPool = tePool env
--       , identMBClientEnv = teMBClientEnv env
--       }
--
--     -- Trigger library scan
--     publishAndLog (teEventBus env) (teLogEnv env) "test" $
--       LibraryScanRequested (toText libPath)
--
--     -- Wait for scan → group → identify chain to complete
--     scanResult <- waitForEventWithTimeout eventChan isMetadataReadComplete (15 * 1000000)
--     assertBool "Scan should complete" (isJust scanResult)
--
--     groupResult <- waitForEventWithTimeout eventChan isClustersGenerated (10 * 1000000)
--     assertBool "Grouping should complete" (isJust groupResult)
--
--     identResult <- waitForEventWithTimeout eventChan isClusterIdentified (20 * 1000000)
--     assertBool "Identification should complete" (isJust identResult)
--
--     -- Verify identification event
--     case identResult of
--       Just (ClusterIdentified{..}) -> do
--         -- Should have identified the cluster with the mocked release
--         identifiedReleaseId @?= "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
--       _ -> assertFailure "Expected ClusterIdentified event"
--
--     -- Verify database was updated with MusicBrainz metadata
--     clusters <- withConnection (tePool env) getAllClusters
--     case clusters of
--       [] -> assertFailure "Expected at least one cluster"
--       (cluster:_) -> do
--         -- Cluster should now have MB IDs populated
--         clusterMBReleaseId cluster @?= Just "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
--         clusterMBReleaseGroupId cluster @?= Just "a1b2c3d4-e5f6-7890-abcd-ef1234567890-rg"
--         clusterMBArtistId cluster @?= Just "381086ea-f511-4aba-bdf9-71c753dc5077"
--
--     -- Cleanup
--     cancel scannerAsync
--     cancel grouperAsync
--     cancel identifierAsync

-- | Test catalog artist workflow: identify → add catalog artist → fetch discography
--
-- testCatalogArtistWorkflow :: IO ()
-- testCatalogArtistWorkflow = withTestEnvAndMocks $ \env mock -> do
--   -- This test verifies that after a cluster is identified:
--   -- 1. The artist is automatically added to the catalog (if not already present)
--   -- 2. A discography fetch is triggered for the artist
--
--   withTempLibraryStructure [mkSimpleAlbum "Kendrick Lamar" "good kid, m.A.A.d city" 12] $ \libPath -> do
--     eventChan <- STM.atomically $ subscribe (teEventBus env)
--
--     -- Mock MusicBrainz responses
--     let mockRelease = mkTestMBRelease
--           "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
--           "good kid, m.A.A.d city"
--           "Kendrick Lamar"
--           "381086ea-f511-4aba-bdf9-71c753dc5077"
--           12
--     let mockArtist = mkTestMBArtist
--           "381086ea-f511-4aba-bdf9-71c753dc5077"
--           "Kendrick Lamar"
--           ["Hip Hop", "Rap"]
--
--     addJSONResponse mock "release?query=" $ mkTestMBReleaseSearch [mockRelease]
--     addJSONResponse mock "artist/381086ea" mockArtist
--
--     -- Start services
--     scannerAsync <- startScannerService $ ScannerDeps { ... }
--     grouperAsync <- startGrouperService $ GrouperDeps { ... }
--     identifierAsync <- startIdentifierService $ IdentifierDeps { ... }
--     catalogAsync <- startCatalogService $ CatalogDeps { ... }
--
--     -- Trigger scan
--     publishAndLog (teEventBus env) (teLogEnv env) "test" $
--       LibraryScanRequested (toText libPath)
--
--     -- Wait for the full chain: scan → group → identify → catalog artist
--     identResult <- waitForEventWithTimeout eventChan isClusterIdentified (30 * 1000000)
--     assertBool "Identification should complete" (isJust identResult)
--
--     catalogResult <- waitForEventWithTimeout eventChan isTrackedArtistAdded (10 * 1000000)
--     assertBool "Artist should be added to catalog" (isJust catalogResult)
--
--     -- Verify catalog artist event
--     case catalogResult of
--       Just (TrackedArtistAdded{ artistMBID = mbid, artistName = name }) -> do
--         mbid @?= "381086ea-f511-4aba-bdf9-71c753dc5077"
--         name @?= "Kendrick Lamar"
--       _ -> assertFailure "Expected TrackedArtistAdded event"
--
--     -- Verify database has catalog artist
--     artists <- withConnection (tePool env) getAllCatalogArtists
--     case artists of
--       [] -> assertFailure "Expected at least one catalog artist"
--       (artist:_) -> do
--         catalogArtistMBID artist @?= "381086ea-f511-4aba-bdf9-71c753dc5077"
--         catalogArtistName artist @?= "Kendrick Lamar"
--
--     -- Verify discography fetch was triggered
--     discoResult <- waitForEventWithTimeout eventChan isArtistDiscographyRequested (5 * 1000000)
--     assertBool "Discography fetch should be requested" (isJust discoResult)
--
--     -- Cleanup
--     cancel scannerAsync
--     cancel grouperAsync
--     cancel identifierAsync
--     cancel catalogAsync

-- | Test discography workflow: fetch artist discography → add catalog albums
--
-- testDiscographyWorkflow :: IO ()
-- testDiscographyWorkflow = withTestEnvAndMocks $ \env mock -> do
--   -- This test verifies that when an artist's discography is fetched:
--   -- 1. All release groups are retrieved from MusicBrainz
--   -- 2. Albums matching configured types (Albums, EPs, etc.) are added to catalog
--   -- 3. Each catalog album emits a CatalogAlbumAdded event
--
--   withTempLibraryStructure [] $ \_ -> do
--     eventChan <- STM.atomically $ subscribe (teEventBus env)
--
--     -- Mock MusicBrainz artist with discography
--     let kendrickMBID = "381086ea-f511-4aba-bdf9-71c753dc5077"
--     let mockReleaseGroups =
--           [ mkTestMBReleaseGroup "rg-gkmc" "good kid, m.A.A.d city" "Album" 2012
--           , mkTestMBReleaseGroup "rg-tpab" "To Pimp a Butterfly" "Album" 2015
--           , mkTestMBReleaseGroup "rg-damn" "DAMN." "Album" 2017
--           ]
--     let mockArtist = mkTestMBArtist kendrickMBID "Kendrick Lamar" ["Hip Hop"]
--           & setArtistReleaseGroups mockReleaseGroups
--
--     addJSONResponse mock ("artist/" <> kendrickMBID) mockArtist
--
--     -- Start catalog service
--     catalogAsync <- startCatalogService $ CatalogDeps { ... }
--
--     -- Manually trigger discography fetch (simulating what happens after artist is added)
--     publishAndLog (teEventBus env) (teLogEnv env) "test" $
--       ArtistDiscographyRequested kendrickMBID "Kendrick Lamar"
--
--     -- Wait for discography to be fetched
--     discoResult <- waitForEventWithTimeout eventChan isArtistDiscographyFetched (15 * 1000000)
--     assertBool "Discography should be fetched" (isJust discoResult)
--
--     -- Verify we got CatalogAlbumAdded events for each album
--     album1 <- waitForEventWithTimeout eventChan isCatalogAlbumAdded (5 * 1000000)
--     album2 <- waitForEventWithTimeout eventChan isCatalogAlbumAdded (5 * 1000000)
--     album3 <- waitForEventWithTimeout eventChan isCatalogAlbumAdded (5 * 1000000)
--
--     assertBool "Should have added 3 albums" (all isJust [album1, album2, album3])
--
--     -- Verify database has all catalog albums
--     albums <- withConnection (tePool env) $ getCatalogAlbumsByArtist kendrickMBID
--     length albums @?= 3
--
--     let albumTitles = sort $ map catalogAlbumTitle albums
--     albumTitles @?= ["DAMN.", "To Pimp a Butterfly", "good kid, m.A.A.d city"]
--
--     -- Cleanup
--     cancel catalogAsync

-- | Test wanted album workflow: catalog album → apply rules → mark wanted → search
--
-- testWantedAlbumWorkflow :: IO ()
-- testWantedAlbumWorkflow = withTestEnvAndMocks $ \env mock -> do
--   -- This test verifies that when a catalog album is added:
--   -- 1. Acquisition rules are evaluated
--   -- 2. If the album matches rules, it's marked as wanted
--   -- 3. A search is triggered for the wanted album
--
--   withTempLibraryStructure [] $ \_ -> do
--     eventChan <- STM.atomically $ subscribe (teEventBus env)
--
--     -- Configure acquisition rules to auto-grab new Kendrick albums
--     config <- STM.readTVarIO (teConfigVar env)
--     let updatedConfig = config
--           & setAcquisitionRules
--               [ AcquisitionRule
--                   { ruleArtistMBID = Just "381086ea-f511-4aba-bdf9-71c753dc5077"
--                   , ruleMonitored = True
--                   , ruleQuality = ["FLAC", "320"]
--                   , ruleReleaseTypes = ["Album"]
--                   }
--               ]
--     STM.atomically $ STM.writeTVar (teConfigVar env) updatedConfig
--
--     -- Start acquisition service
--     acqAsync <- startAcquisitionService $ AcquisitionDeps { ... }
--
--     -- Manually add a catalog album (simulating what happens after discography fetch)
--     publishAndLog (teEventBus env) (teLogEnv env) "test" $
--       CatalogAlbumAdded
--         { albumMBReleaseGroupID = "rg-damn"
--         , albumTitle = "DAMN."
--         , albumArtistMBID = "381086ea-f511-4aba-bdf9-71c753dc5077"
--         , albumArtistName = "Kendrick Lamar"
--         , albumYear = Just 2017
--         , albumType = "Album"
--         }
--
--     -- Wait for wanted album to be added
--     wantedResult <- waitForEventWithTimeout eventChan isWantedAlbumAdded (10 * 1000000)
--     assertBool "Album should be marked wanted" (isJust wantedResult)
--
--     -- Verify search was triggered
--     searchResult <- waitForEventWithTimeout eventChan isAlbumSearchStarted (5 * 1000000)
--     assertBool "Search should be triggered" (isJust searchResult)
--
--     case searchResult of
--       Just (AlbumSearchStarted{ searchAlbumTitle = title }) ->
--         title @?= "DAMN."
--       _ -> assertFailure "Expected AlbumSearchStarted event"
--
--     -- Verify database has wanted album
--     wanted <- withConnection (tePool env) getAllWantedAlbums
--     case wanted of
--       [] -> assertFailure "Expected at least one wanted album"
--       (album:_) ->
--         wantedAlbumReleaseGroupID album @?= "rg-damn"
--
--     -- Cleanup
--     cancel acqAsync

-- | Test download and import workflow: search → download → import
--
-- testDownloadImportWorkflow :: IO ()
-- testDownloadImportWorkflow = withTestEnvAndMocks $ \env mock -> do
--   -- This test verifies the complete download and import flow:
--   -- 1. Search indexers for the album
--   -- 2. Evaluate sources and select best match
--   -- 3. Queue download with download client
--   -- 4. Monitor download progress
--   -- 5. When complete, import to library
--   -- 6. Verify imported album is in library with correct metadata
--
--   withTempLibraryStructure [] $ \_ -> do
--     eventChan <- STM.atomically $ subscribe (teEventBus env)
--
--     -- Mock indexer search results
--     let mockIndexerResults =
--           [ IndexerResult
--               { resultTitle = "Kendrick Lamar - DAMN. (2017) [FLAC]"
--               , resultSize = 350 * 1024 * 1024  -- 350MB
--               , resultSeeders = 150
--               , resultQuality = "FLAC"
--               , resultDownloadUrl = "magnet:?xt=..."
--               , resultIndexer = "TestIndexer"
--               }
--           ]
--
--     addJSONResponse mock "search?q=DAMN" mockIndexerResults
--
--     -- Mock download client (SABnzbd/Transmission/qBittorrent)
--     addJSONResponse mock "transmission/rpc" $ TransmissionResponse { success = True }
--
--     -- Start download service
--     downloadAsync <- startDownloadService $ DownloadDeps { ... }
--     importerAsync <- startImporterService $ ImporterDeps { ... }
--
--     -- Trigger album search (simulating what happens when album is marked wanted)
--     publishAndLog (teEventBus env) (teLogEnv env) "test" $
--       AlbumSearchStarted
--         { searchAlbumTitle = "DAMN."
--         , searchArtistName = "Kendrick Lamar"
--         , searchReleaseGroupID = "rg-damn"
--         }
--
--     -- Wait for download to be queued
--     queueResult <- waitForEventWithTimeout eventChan isDownloadQueued (10 * 1000000)
--     assertBool "Download should be queued" (isJust queueResult)
--
--     -- Simulate download completing (in real test, would monitor actual download)
--     -- For testing, manually trigger import
--     publishAndLog (teEventBus env) (teLogEnv env) "test" $
--       DownloadCompleted
--         { downloadPath = "/downloads/Kendrick.Lamar.DAMN.2017.FLAC"
--         , downloadReleaseGroupID = "rg-damn"
--         }
--
--     -- Wait for import to complete
--     importResult <- waitForEventWithTimeout eventChan isDownloadImported (20 * 1000000)
--     assertBool "Download should be imported" (isJust importResult)
--
--     -- Verify album is now in library
--     clusters <- withConnection (tePool env) getAllClusters
--     case filter (\c -> clusterAlbum c == Just "DAMN.") clusters of
--       [] -> assertFailure "Imported album not found in library"
--       (cluster:_) -> do
--         clusterAlbum cluster @?= Just "DAMN."
--         clusterAlbumArtist cluster @?= Just "Kendrick Lamar"
--         clusterMBReleaseGroupId cluster @?= Just "rg-damn"
--
--     -- Verify wanted album was marked as imported
--     wanted <- withConnection (tePool env) $ getWantedAlbumByReleaseGroup "rg-damn"
--     case wanted of
--       Nothing -> assertFailure "Wanted album not found"
--       Just album ->
--         wantedAlbumStatus album @?= "Imported"
--
--     -- Cleanup
--     cancel downloadAsync
--     cancel importerAsync
