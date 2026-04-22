{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Identification pure logic, focused on
-- 'buildFileGroup' (cluster + tracks -> FileGroup aggregation).
module Unit.Domain.IdentificationSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.Domain.Identification
import Skema.Database.Types (ClusterRecord(..), LibraryTrackMetadataRecord(..))
import Skema.MusicBrainz.Types (FileGroup(..), MBID(..))
import System.OsPath (OsPath, encodeUtf, takeDirectory)
import Helpers.Builders (mkTestMetadataRecord)

-- =============================================================================
-- Builders
-- =============================================================================

emptyCluster :: ClusterRecord
emptyCluster = ClusterRecord
  { clusterId = Just 1
  , clusterMetadataHash = "h"
  , clusterAlbum = Just "Kid A"
  , clusterAlbumArtist = Just "Radiohead"
  , clusterTrackCount = 0
  , clusterMBReleaseId = Nothing
  , clusterMBReleaseGroupId = Nothing
  , clusterMBConfidence = Nothing
  , clusterCreatedAt = Nothing
  , clusterUpdatedAt = Nothing
  , clusterLastIdentifiedAt = Nothing
  , clusterMBReleaseData = Nothing
  , clusterMBCandidates = Nothing
  , clusterMatchSource = Nothing
  , clusterMatchLocked = False
  , clusterQuality = Nothing
  }

path :: String -> OsPath
path s = case encodeUtf s of
  Just p -> p
  Nothing -> error "invalid path in test"

-- | Build a track row with given path and metadata.
trackRow
  :: Int
  -> String
  -> LibraryTrackMetadataRecord
  -> (Int64, OsPath, LibraryTrackMetadataRecord, Maybe Text, Maybe Text)
trackRow tid p meta = (fromIntegral tid, path p, meta, Nothing, Nothing)

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Identification"
  [ buildFileGroupTests
  ]

buildFileGroupTests :: TestTree
buildFileGroupTests = testGroup "buildFileGroup"
  [ testCase "empty track list -> Nothing" $
      buildFileGroup emptyCluster [] @?= Nothing

  , testCase "single track populates directory and basic fields" $ do
      let meta = mkTestMetadataRecord "Kid A" "Radiohead" 1
          tracks = [trackRow 1 "/music/Radiohead/Kid A/01.flac" meta]
      case buildFileGroup emptyCluster tracks of
        Nothing -> assertFailure "expected Just FileGroup"
        Just fg -> do
          fgDirectory fg @?= takeDirectory (path "/music/Radiohead/Kid A/01.flac")
          fgAlbum fg @?= Just "Kid A"
          fgArtist fg @?= Just "Radiohead"
          length (fgFiles fg) @?= 1

  , testCase "directory taken from first track only" $ do
      let meta = mkTestMetadataRecord "A" "B" 1
          tracks =
            [ trackRow 1 "/music/first/01.flac" meta
            , trackRow 2 "/different/second/02.flac" meta
            ]
      case buildFileGroup emptyCluster tracks of
        Just fg -> fgDirectory fg @?= takeDirectory (path "/music/first/01.flac")
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "first-non-Nothing aggregation for label" $ do
      -- Track 1 has no label; track 2 does. Result should use track 2's value.
      let meta1 = (mkTestMetadataRecord "A" "B" 1) { metaLabel = Nothing }
          meta2 = (mkTestMetadataRecord "A" "B" 2) { metaLabel = Just "XL Recordings" }
          tracks =
            [ trackRow 1 "/p/01.flac" meta1
            , trackRow 2 "/p/02.flac" meta2
            ]
      case buildFileGroup emptyCluster tracks of
        Just fg -> fgLabel fg @?= Just "XL Recordings"
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "first-non-Nothing picks earliest track that has the value" $ do
      let meta1 = (mkTestMetadataRecord "A" "B" 1) { metaLabel = Just "First" }
          meta2 = (mkTestMetadataRecord "A" "B" 2) { metaLabel = Just "Second" }
          tracks =
            [ trackRow 1 "/p/01.flac" meta1
            , trackRow 2 "/p/02.flac" meta2
            ]
      case buildFileGroup emptyCluster tracks of
        Just fg -> fgLabel fg @?= Just "First"
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "all optional fields Nothing when no track supplies them" $ do
      let meta = mkTestMetadataRecord "A" "B" 1
          tracks = [trackRow 1 "/p/01.flac" meta]
      case buildFileGroup emptyCluster tracks of
        Just fg -> do
          fgLabel fg @?= Nothing
          fgCatalogNumber fg @?= Nothing
          fgBarcode fg @?= Nothing
          fgCountry fg @?= Nothing
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "MusicBrainz release IDs come from first track's tags" $ do
      let meta1 = (mkTestMetadataRecord "A" "B" 1)
            { metaMBReleaseId = Just "release-xyz"
            , metaMBReleaseGroupId = Just "rg-xyz"
            }
          meta2 = (mkTestMetadataRecord "A" "B" 2)
            { metaMBReleaseId = Just "IGNORED-later-track-value"
            , metaMBReleaseGroupId = Just "IGNORED-later-track-value"
            }
          tracks =
            [ trackRow 1 "/p/01.flac" meta1
            , trackRow 2 "/p/02.flac" meta2
            ]
      case buildFileGroup emptyCluster tracks of
        Just fg -> do
          fgReleaseId fg @?= Just (MBID "release-xyz")
          fgReleaseGroupId fg @?= Just (MBID "rg-xyz")
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "MBIDs Nothing when first track has none" $ do
      -- Aggregation rule for MBIDs uses first track only; a later track's
      -- value does not substitute.
      let meta1 = (mkTestMetadataRecord "A" "B" 1) { metaMBReleaseId = Nothing }
          meta2 = (mkTestMetadataRecord "A" "B" 2) { metaMBReleaseId = Just "rel-2" }
          tracks =
            [ trackRow 1 "/p/01.flac" meta1
            , trackRow 2 "/p/02.flac" meta2
            ]
      case buildFileGroup emptyCluster tracks of
        Just fg -> fgReleaseId fg @?= Nothing
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "album/artist always from cluster, not tracks" $ do
      let cluster = emptyCluster
            { clusterAlbum = Just "From Cluster"
            , clusterAlbumArtist = Just "Cluster Artist"
            }
          meta = (mkTestMetadataRecord "Track Album" "Track Artist" 1)
          tracks = [trackRow 1 "/p/01.flac" meta]
      case buildFileGroup cluster tracks of
        Just fg -> do
          fgAlbum fg @?= Just "From Cluster"
          fgArtist fg @?= Just "Cluster Artist"
        Nothing -> assertFailure "expected Just FileGroup"
  ]
