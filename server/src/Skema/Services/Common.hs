{-# LANGUAGE OverloadedStrings #-}

-- | Common utilities shared across services.
module Skema.Services.Common
  ( metadataRecordToMonatone
  ) where

import Skema.Database.Types (LibraryTrackMetadataRecord(..))
import qualified Monatone.Metadata as M

-- | Convert LibraryTrackMetadataRecord to Monatone Metadata.
--
-- This function converts database metadata records to the Monatone format
-- used for MusicBrainz identification and diff generation.
metadataRecordToMonatone :: LibraryTrackMetadataRecord -> M.Metadata
metadataRecordToMonatone meta = M.Metadata
  { M.format = maybe M.MP3 parseFormat (metaFormat meta)  -- Default to MP3 if not set
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
  , M.albumArtInfo = Nothing  -- Album art stored separately, not in metadata record
  , M.audioProperties = M.AudioProperties
      { M.duration = fmap (\secs -> round (secs * 1000.0)) (metaDurationSeconds meta)
      , M.bitrate = Nothing  -- Bitrate not stored in metadata, would need audio file analysis
      , M.sampleRate = Nothing  -- Sample rate not stored in metadata, would need audio file analysis
      , M.channels = Nothing  -- Channels not stored in metadata, would need audio file analysis
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
  , M.rawTags = mempty  -- Raw tags not stored in database
  }

-- | Parse audio format from text.
parseFormat :: Text -> M.AudioFormat
parseFormat "FLAC" = M.FLAC
parseFormat "OGG" = M.OGG
parseFormat "Opus" = M.Opus
parseFormat "MP3" = M.MP3
parseFormat _ = M.MP3  -- Default to MP3 for unknown formats
