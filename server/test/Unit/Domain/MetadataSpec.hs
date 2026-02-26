{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Metadata pure metadata comparison logic.
--
-- Tests:
-- - normalizeEmpty: empty string normalization
-- - compareTextField: text field comparison with normalization
-- - computeMetadataDiffs: full metadata diff computation
module Unit.Domain.MetadataSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Skema.Domain.Metadata
import qualified Monatone.Metadata as M
import Helpers.Builders (mkTestMBRelease, mkTestMBTrack)
import qualified Data.HashMap.Strict as HM

-- =============================================================================
-- Test Helpers
-- =============================================================================

-- | Create a test Metadata with the given title and artist.
mkTestMetadata :: Maybe Text -> Maybe Text -> M.Metadata
mkTestMetadata titleVal artistVal = M.Metadata
  { M.format = M.FLAC
  , M.title = titleVal
  , M.artist = artistVal
  , M.album = Just "Test Album"
  , M.albumArtist = Just "Test Artist"
  , M.trackNumber = Just 1
  , M.totalTracks = Nothing
  , M.discNumber = Just 1
  , M.totalDiscs = Nothing
  , M.date = Just "2024-01-15"
  , M.year = Just 2024
  , M.genre = Nothing
  , M.publisher = Nothing
  , M.comment = Nothing
  , M.releaseCountry = Just "US"
  , M.recordLabel = Just "Test Records"
  , M.catalogNumber = Just "TEST-001"
  , M.barcode = Nothing
  , M.releaseStatus = Nothing
  , M.releaseType = Nothing
  , M.albumArtInfo = Nothing
  , M.audioProperties = M.emptyAudioProperties
  , M.musicBrainzIds = M.emptyMusicBrainzIds
  , M.acoustidFingerprint = Nothing
  , M.acoustidId = Nothing
  , M.rawTags = HM.empty
  }

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Metadata"
  [ normalizeEmptyTests
  , compareTextFieldTests
  , computeMetadataDiffsTests
  ]

-- =============================================================================
-- normalizeEmpty Tests
-- =============================================================================

normalizeEmptyTests :: TestTree
normalizeEmptyTests = testGroup "normalizeEmpty"
  [ testCase "Nothing -> Nothing" $
      normalizeEmpty Nothing @?= Nothing

  , testCase "Just empty string -> Nothing" $
      normalizeEmpty (Just "") @?= Nothing

  , testCase "Just whitespace-padded string -> Just trimmed" $
      normalizeEmpty (Just " hello ") @?= Just "hello"
  ]

-- =============================================================================
-- compareTextField Tests
-- =============================================================================

compareTextFieldTests :: TestTree
compareTextFieldTests = testGroup "compareTextField"
  [ testCase "same values -> Nothing (no diff)" $
      compareTextField "title" (Just "Hello") (Just "Hello") @?= Nothing

  , testCase "different values -> Just diff" $ do
      let result = compareTextField "title" (Just "Hello") (Just "World")
      case result of
        Just ("title", Just "Hello", Just "World") -> pure ()
        other -> assertFailure $ "Expected title diff, got: " <> show other

  , testCase "both empty -> Nothing (no diff)" $
      compareTextField "title" Nothing Nothing @?= Nothing
  ]

-- =============================================================================
-- computeMetadataDiffs Tests
-- =============================================================================

computeMetadataDiffsTests :: TestTree
computeMetadataDiffsTests = testGroup "computeMetadataDiffs"
  [ testCase "detects title diff" $ do
      let fileMeta = mkTestMetadata (Just "Wrong Title") (Just "Test Artist")
          mbTrack = mkTestMBTrack 1 "Correct Title" 180000
          mbRelease = mkTestMBRelease "release-1" "Test Album" "Test Artist" "artist-1" 1
          diffs = computeMetadataDiffs fileMeta mbTrack mbRelease
          titleDiffs = filter (\(field, _, _) -> field == "title") diffs
      assertBool "Should have a title diff" (not (null titleDiffs))

  , testCase "no diff when values match" $ do
      let fileMeta = mkTestMetadata (Just "Track 1") Nothing
          mbTrack = mkTestMBTrack 1 "Track 1" 180000
          mbRelease = mkTestMBRelease "release-1" "Test Album" "Test Artist" "artist-1" 1
          diffs = computeMetadataDiffs fileMeta mbTrack mbRelease
          titleDiffs = filter (\(field, _, _) -> field == "title") diffs
      assertBool "Should not have a title diff" (null titleDiffs)
  ]
