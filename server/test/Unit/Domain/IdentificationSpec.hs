{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Identification pure logic, focused on
-- 'buildFileGroup' (album info + per-file metadata -> FileGroup aggregation).
module Unit.Domain.IdentificationSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.Domain.Identification
import Skema.Database.Repository (metadataRecordToMonatone)
import Skema.Database.Types (LibraryTrackMetadataRecord(..))
import Skema.Domain.Matching (FileGroup(..))
import Skema.MusicBrainz.Types (MBID(..))
import qualified Monatone.Metadata as M
import System.OsPath (OsPath, encodeUtf, takeDirectory)
import Helpers.Builders (mkTestMetadataRecord)

-- =============================================================================
-- Builders
-- =============================================================================

path :: String -> OsPath
path s = case encodeUtf s of
  Just p -> p
  Nothing -> error "invalid path in test"

-- | Build a (path, metadata) file entry from a persisted metadata record.
fileEntry :: String -> LibraryTrackMetadataRecord -> (OsPath, M.Metadata)
fileEntry p meta = (path p, metadataRecordToMonatone meta)

clusterAlbum :: Maybe Text
clusterAlbum = Just "Kid A"

clusterArtist :: Maybe Text
clusterArtist = Just "Radiohead"

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Identification"
  [ buildFileGroupTests
  ]

buildFileGroupTests :: TestTree
buildFileGroupTests = testGroup "buildFileGroup"
  [ testCase "empty file list -> Nothing" $
      buildFileGroup clusterAlbum clusterArtist [] @?= Nothing

  , testCase "single file populates directory and basic fields" $ do
      let meta = mkTestMetadataRecord "Kid A" "Radiohead" 1
          files = [fileEntry "/music/Radiohead/Kid A/01.flac" meta]
      case buildFileGroup clusterAlbum clusterArtist files of
        Nothing -> assertFailure "expected Just FileGroup"
        Just fg -> do
          fgDirectory fg @?= takeDirectory (path "/music/Radiohead/Kid A/01.flac")
          fgAlbum fg @?= Just "Kid A"
          fgArtist fg @?= Just "Radiohead"
          length (fgFiles fg) @?= 1

  , testCase "directory taken from first file only" $ do
      let meta = mkTestMetadataRecord "A" "B" 1
          files =
            [ fileEntry "/music/first/01.flac" meta
            , fileEntry "/different/second/02.flac" meta
            ]
      case buildFileGroup clusterAlbum clusterArtist files of
        Just fg -> fgDirectory fg @?= takeDirectory (path "/music/first/01.flac")
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "first-non-Nothing aggregation for label" $ do
      -- File 1 has no label; file 2 does. Result should use file 2's value.
      let meta1 = (mkTestMetadataRecord "A" "B" 1) { metaLabel = Nothing }
          meta2 = (mkTestMetadataRecord "A" "B" 2) { metaLabel = Just "XL Recordings" }
          files =
            [ fileEntry "/p/01.flac" meta1
            , fileEntry "/p/02.flac" meta2
            ]
      case buildFileGroup clusterAlbum clusterArtist files of
        Just fg -> fgLabel fg @?= Just "XL Recordings"
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "first-non-Nothing picks earliest file that has the value" $ do
      let meta1 = (mkTestMetadataRecord "A" "B" 1) { metaLabel = Just "First" }
          meta2 = (mkTestMetadataRecord "A" "B" 2) { metaLabel = Just "Second" }
          files =
            [ fileEntry "/p/01.flac" meta1
            , fileEntry "/p/02.flac" meta2
            ]
      case buildFileGroup clusterAlbum clusterArtist files of
        Just fg -> fgLabel fg @?= Just "First"
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "all optional fields Nothing when no file supplies them" $ do
      let meta = mkTestMetadataRecord "A" "B" 1
          files = [fileEntry "/p/01.flac" meta]
      case buildFileGroup clusterAlbum clusterArtist files of
        Just fg -> do
          fgLabel fg @?= Nothing
          fgCatalogNumber fg @?= Nothing
          fgBarcode fg @?= Nothing
          fgCountry fg @?= Nothing
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "MusicBrainz release IDs come from first file's tags" $ do
      let meta1 = (mkTestMetadataRecord "A" "B" 1)
            { metaMBReleaseId = Just "release-xyz"
            , metaMBReleaseGroupId = Just "rg-xyz"
            }
          meta2 = (mkTestMetadataRecord "A" "B" 2)
            { metaMBReleaseId = Just "IGNORED-later-track-value"
            , metaMBReleaseGroupId = Just "IGNORED-later-track-value"
            }
          files =
            [ fileEntry "/p/01.flac" meta1
            , fileEntry "/p/02.flac" meta2
            ]
      case buildFileGroup clusterAlbum clusterArtist files of
        Just fg -> do
          fgReleaseId fg @?= Just (MBID "release-xyz")
          fgReleaseGroupId fg @?= Just (MBID "rg-xyz")
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "MBIDs Nothing when first file has none" $ do
      -- Aggregation rule for MBIDs uses first file only; a later file's
      -- value does not substitute.
      let meta1 = (mkTestMetadataRecord "A" "B" 1) { metaMBReleaseId = Nothing }
          meta2 = (mkTestMetadataRecord "A" "B" 2) { metaMBReleaseId = Just "rel-2" }
          files =
            [ fileEntry "/p/01.flac" meta1
            , fileEntry "/p/02.flac" meta2
            ]
      case buildFileGroup clusterAlbum clusterArtist files of
        Just fg -> fgReleaseId fg @?= Nothing
        Nothing -> assertFailure "expected Just FileGroup"

  , testCase "album/artist always from the supplied cluster info, not files" $ do
      let meta = mkTestMetadataRecord "Track Album" "Track Artist" 1
          files = [fileEntry "/p/01.flac" meta]
      case buildFileGroup (Just "From Cluster") (Just "Cluster Artist") files of
        Just fg -> do
          fgAlbum fg @?= Just "From Cluster"
          fgArtist fg @?= Just "Cluster Artist"
        Nothing -> assertFailure "expected Just FileGroup"
  ]
