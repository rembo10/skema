{-# LANGUAGE OverloadedStrings #-}

-- | Pure metadata business logic.
--
-- This module contains domain logic for metadata comparison, diff computation,
-- and validation. All functions are pure with no IO or database operations.
module Skema.Domain.Metadata
  ( -- * Diff Computation
    computeMetadataDiffs
  , MetadataDiff(..)
    -- * Comparison Helpers
  , normalizeEmpty
  , compareField
  , compareTextField
    -- * Record Conversions
  , metadataRecordToMonatone
  , parseAudioFormat
  ) where

import Monatone.Metadata (Metadata(..))
import qualified Monatone.Metadata as M
import Skema.Database.Types (LibraryTrackMetadataRecord(..))
import Skema.MusicBrainz.Types (MBTrack(..), MBRelease(..))
import qualified Data.Text as T

-- | A metadata diff between file and MusicBrainz data.
data MetadataDiff = MetadataDiff
  { diffFieldName :: Text
  , diffFileValue :: Maybe Text
  , diffMBValue :: Maybe Text
  } deriving (Show, Eq)

-- | Compute metadata differences between file and MusicBrainz data.
--
-- This is pure domain logic that compares file metadata against MusicBrainz
-- canonical data and returns a list of fields that differ.
--
-- Comparison rules:
-- - Empty string is treated as Nothing
-- - Whitespace is trimmed before comparison
-- - Both Nothing is not a diff (no change needed)
-- - Only creates diffs when values actually differ
computeMetadataDiffs :: Metadata -> MBTrack -> MBRelease -> [(Text, Maybe Text, Maybe Text)]
computeMetadataDiffs fileMeta mbTrack mbRelease =
  let
    -- Track-level comparisons
    titleDiff = compareTextField "title" (M.title fileMeta) (Just $ mbTrackTitle mbTrack)
    trackNumDiff = compareField "track_number" (M.trackNumber fileMeta) (Just $ mbTrackPosition mbTrack) show
    trackArtistDiff = compareTextField "track_artist" (M.artist fileMeta) (mbTrackArtist mbTrack)

    -- Release-level comparisons
    albumDiff = compareTextField "album" (M.album fileMeta) (Just $ mbReleaseTitle mbRelease)
    albumArtistDiff = compareTextField "album_artist" (M.albumArtist fileMeta) (Just $ mbReleaseArtist mbRelease)
    dateDiff = compareTextField "date" (M.date fileMeta) (mbReleaseDate mbRelease)
    yearDiff = compareField "year" (M.year fileMeta) (mbReleaseYear mbRelease) show
    countryDiff = compareTextField "country" (M.releaseCountry fileMeta) (mbReleaseCountry mbRelease)
    labelDiff = compareTextField "label" (M.recordLabel fileMeta) (mbReleaseLabel mbRelease)
    catalogDiff = compareTextField "catalog_number" (M.catalogNumber fileMeta) (mbReleaseCatalogNumber mbRelease)
    barcodeDiff = compareTextField "barcode" (M.barcode fileMeta) (mbReleaseBarcode mbRelease)

    -- Genre comparison (MusicBrainz provides a list, join with semicolons for comparison)
    genreDiff = compareTextField "genre" (M.genre fileMeta)
      (if null (mbReleaseGenres mbRelease)
       then Nothing
       else Just (T.intercalate "; " (mbReleaseGenres mbRelease)))

  in catMaybes
    [ titleDiff
    , trackNumDiff
    , trackArtistDiff
    , albumDiff
    , albumArtistDiff
    , dateDiff
    , yearDiff
    , countryDiff
    , labelDiff
    , catalogDiff
    , barcodeDiff
    , genreDiff
    ]

-- | Normalize empty strings to Nothing.
--
-- This ensures that "" and Nothing are treated as equivalent (both empty).
-- Also strips whitespace for consistent comparison.
normalizeEmpty :: Maybe Text -> Maybe Text
normalizeEmpty Nothing = Nothing
normalizeEmpty (Just "") = Nothing
normalizeEmpty (Just t) = Just (T.strip t)

-- | Compare two values and create diff if different.
--
-- Skip if both values are Nothing (empty-to-empty should not create a diff).
-- Returns Nothing if values are equal, Just diff if they differ.
compareField :: Eq a => Text -> Maybe a -> Maybe a -> (a -> Text) -> Maybe (Text, Maybe Text, Maybe Text)
compareField fieldName fileVal mbVal showFn =
  case (fileVal, mbVal) of
    (Nothing, Nothing) -> Nothing  -- Both empty, no diff
    _ | fileVal == mbVal -> Nothing  -- Same value, no diff
    _ -> Just (fieldName, fmap showFn fileVal, fmap showFn mbVal)  -- Different, create diff

-- | Compare two Text fields with normalization.
--
-- Normalizes empty strings to Nothing before comparison.
compareTextField :: Text -> Maybe Text -> Maybe Text -> Maybe (Text, Maybe Text, Maybe Text)
compareTextField fieldName fileVal mbVal =
  let fileVal' = normalizeEmpty fileVal
      mbVal' = normalizeEmpty mbVal
  in case (fileVal', mbVal') of
    (Nothing, Nothing) -> Nothing  -- Both empty, no diff
    _ | fileVal' == mbVal' -> Nothing  -- Same value, no diff
    _ -> Just (fieldName, fileVal', mbVal')  -- Different, create diff

-- | Parse an audio format string into the Monatone format enum.
-- Unknown strings fall back to 'M.MP3'.
parseAudioFormat :: Text -> M.AudioFormat
parseAudioFormat "FLAC" = M.FLAC
parseAudioFormat "OGG"  = M.OGG
parseAudioFormat "Opus" = M.Opus
parseAudioFormat "MP3"  = M.MP3
parseAudioFormat _      = M.MP3

-- | Convert a persisted 'LibraryTrackMetadataRecord' to the Monatone
-- 'Metadata' shape used for MusicBrainz identification and diff generation.
--
-- Fields not represented in the persisted record (album art, raw tags,
-- bitrate/sample rate/channels) are emitted as 'Nothing' / 'mempty'.
-- Duration is converted from seconds (stored) to milliseconds (Monatone).
metadataRecordToMonatone :: LibraryTrackMetadataRecord -> M.Metadata
metadataRecordToMonatone meta = M.Metadata
  { M.format = maybe M.MP3 parseAudioFormat (metaFormat meta)
  , M.title = metaTitle meta
  , M.artist = metaArtist meta
  , M.album = metaAlbum meta
  , M.albumArtist = metaAlbumArtist meta
  , M.trackNumber = metaTrackNumber meta
  , M.totalTracks = metaTotalTracks meta
  , M.discNumber = metaDiscNumber meta
  , M.totalDiscs = metaTotalDiscs meta
  , M.date = metaDate meta
  , M.year = metaYear meta
  , M.genre = metaGenre meta
  , M.publisher = metaPublisher meta
  , M.comment = metaComment meta
  , M.releaseCountry = metaCountry meta
  , M.recordLabel = metaLabel meta
  , M.catalogNumber = metaCatalogNumber meta
  , M.barcode = metaBarcode meta
  , M.releaseStatus = metaReleaseStatus meta
  , M.releaseType = metaReleaseType meta
  , M.albumArtInfo = Nothing
  , M.audioProperties = M.AudioProperties
      { M.duration = fmap (\secs -> round (secs * 1000.0)) (metaDurationSeconds meta)
      , M.bitrate = Nothing
      , M.sampleRate = Nothing
      , M.channels = Nothing
      , M.bitsPerSample = metaBitsPerSample meta
      }
  , M.musicBrainzIds = M.MusicBrainzIds
      { M.mbRecordingId = metaMBRecordingId meta
      , M.mbTrackId = metaMBTrackId meta
      , M.mbReleaseId = metaMBReleaseId meta
      , M.mbArtistId = metaMBArtistId meta
      , M.mbAlbumArtistId = metaMBAlbumArtistId meta
      , M.mbReleaseGroupId = metaMBReleaseGroupId meta
      , M.mbWorkId = metaMBWorkId meta
      , M.mbDiscId = metaMBDiscId meta
      }
  , M.acoustidFingerprint = metaAcoustidFingerprint meta
  , M.acoustidId = metaAcoustidId meta
  , M.rawTags = mempty
  }
