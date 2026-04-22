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
import Skema.Database.Types (LibraryTrackMetadataRecord(..))
import qualified Monatone.Metadata as M
import Helpers.Builders (mkTestMBRelease, mkTestMBTrack, mkTestMetadataRecord)
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
  , parseAudioFormatTests
  , metadataRecordToMonatoneTests
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

-- =============================================================================
-- parseAudioFormat Tests
-- =============================================================================

parseAudioFormatTests :: TestTree
parseAudioFormatTests = testGroup "parseAudioFormat"
  [ testCase "FLAC -> FLAC" $ parseAudioFormat "FLAC" @?= M.FLAC
  , testCase "OGG -> OGG"   $ parseAudioFormat "OGG"  @?= M.OGG
  , testCase "Opus -> Opus" $ parseAudioFormat "Opus" @?= M.Opus
  , testCase "MP3 -> MP3"   $ parseAudioFormat "MP3"  @?= M.MP3
  , testCase "unknown text defaults to MP3" $
      parseAudioFormat "gibberish" @?= M.MP3
  , testCase "case-sensitive: flac (lowercase) defaults to MP3" $
      parseAudioFormat "flac" @?= M.MP3
  ]

-- =============================================================================
-- metadataRecordToMonatone Tests
-- =============================================================================

metadataRecordToMonatoneTests :: TestTree
metadataRecordToMonatoneTests = testGroup "metadataRecordToMonatone"
  [ testCase "copies basic track fields through" $ do
      let rec = mkTestMetadataRecord "Test Album" "Test Artist" 3
          meta = metadataRecordToMonatone rec
      M.title meta @?= Just "Track 3"
      M.artist meta @?= Just "Test Artist"
      M.album meta @?= Just "Test Album"
      M.trackNumber meta @?= Just 3

  , testCase "missing format field defaults to MP3" $ do
      let rec = mkTestMetadataRecord "A" "B" 1
          meta = metadataRecordToMonatone rec
      M.format meta @?= M.MP3

  , testCase "FLAC format string parses to FLAC" $ do
      let rec = (mkTestMetadataRecord "A" "B" 1) { metaFormat = Just "FLAC" }
          meta = metadataRecordToMonatone rec
      M.format meta @?= M.FLAC

  , testCase "duration converts seconds to milliseconds" $ do
      let rec = (mkTestMetadataRecord "A" "B" 1) { metaDurationSeconds = Just 180.5 }
          meta = metadataRecordToMonatone rec
      M.duration (M.audioProperties meta) @?= Just 180500

  , testCase "missing duration -> Nothing" $ do
      let rec = (mkTestMetadataRecord "A" "B" 1) { metaDurationSeconds = Nothing }
          meta = metadataRecordToMonatone rec
      M.duration (M.audioProperties meta) @?= Nothing

  , testCase "bitrate/sampleRate/channels always Nothing (not stored)" $ do
      let rec = mkTestMetadataRecord "A" "B" 1
          props = M.audioProperties (metadataRecordToMonatone rec)
      M.bitrate props @?= Nothing
      M.sampleRate props @?= Nothing
      M.channels props @?= Nothing

  , testCase "MusicBrainz IDs round-trip" $ do
      let rec = (mkTestMetadataRecord "A" "B" 1)
            { metaMBRecordingId = Just "rec-id"
            , metaMBReleaseId = Just "rel-id"
            , metaMBArtistId = Just "art-id"
            }
          mbIds = M.musicBrainzIds (metadataRecordToMonatone rec)
      M.mbRecordingId mbIds @?= Just "rec-id"
      M.mbReleaseId mbIds @?= Just "rel-id"
      M.mbArtistId mbIds @?= Just "art-id"

  , testCase "rawTags is empty (not persisted in DB)" $ do
      let rec = mkTestMetadataRecord "A" "B" 1
          meta = metadataRecordToMonatone rec
      HM.null (M.rawTags meta) @?= True
  ]
