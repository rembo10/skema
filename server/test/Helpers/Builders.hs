{-# LANGUAGE OverloadedStrings #-}

-- | Test data builders for creating test fixtures.
--
-- Provides convenient functions to create test tracks, albums,
-- metadata, and other domain objects with sensible defaults.
module Helpers.Builders
  ( -- * Metadata Builders
    mkTestMetadataRecord
    -- * MusicBrainz Builders
  , mkTestMBArtist
  , mkTestMBReleaseGroup
  , mkTestMBRelease
  , mkTestMBTrack
  , mkTestMBReleaseSearch
  ) where

import Skema.Database.Types
import Skema.MusicBrainz.Types

-- | Create test metadata with sensible defaults.
mkTestMetadataRecord
  :: Text  -- ^ Album title
  -> Text  -- ^ Album artist
  -> Int   -- ^ Track number
  -> LibraryTrackMetadataRecord
mkTestMetadataRecord album albumArtist trackNum = LibraryTrackMetadataRecord
  { metaTrackId = Nothing
  , metaFormat = Nothing
  , metaTitle = Just $ "Track " <> show trackNum
  , metaArtist = Just albumArtist
  , metaAlbum = Just album
  , metaAlbumArtist = Just albumArtist
  , metaTrackNumber = Just trackNum
  , metaTotalTracks = Nothing
  , metaDiscNumber = Just 1
  , metaTotalDiscs = Nothing
  , metaDate = Nothing
  , metaYear = Just 2024
  , metaGenre = Nothing
  , metaPublisher = Nothing
  , metaComment = Nothing
  , metaDurationSeconds = Just 180.0
  , metaBitsPerSample = Nothing
  , metaCountry = Nothing
  , metaLabel = Nothing
  , metaCatalogNumber = Nothing
  , metaBarcode = Nothing
  , metaReleaseStatus = Nothing
  , metaReleaseType = Nothing
  , metaMBRecordingId = Nothing
  , metaMBTrackId = Nothing
  , metaMBReleaseId = Nothing
  , metaMBReleaseGroupId = Nothing
  , metaMBArtistId = Nothing
  , metaMBAlbumArtistId = Nothing
  , metaMBWorkId = Nothing
  , metaMBDiscId = Nothing
  , metaAcoustidFingerprint = Nothing
  , metaAcoustidId = Nothing
  , metaCreatedAt = Nothing
  , metaUpdatedAt = Nothing
  }

-- | Create a test MusicBrainz artist with release groups.
mkTestMBArtist :: Text -> Text -> [MBReleaseGroup] -> MBArtist
mkTestMBArtist mbid name releaseGroups = MBArtist
  { mbArtistId = MBID mbid
  , mbArtistName = name
  , mbArtistReleaseGroups = releaseGroups
  }

-- | Create a test MusicBrainz release group.
mkTestMBReleaseGroup :: Text -> Text -> Maybe Text -> Maybe Text -> MBReleaseGroup
mkTestMBReleaseGroup mbid title albumType firstReleaseDate = MBReleaseGroup
  { mbrgId = MBID mbid
  , mbrgTitle = title
  , mbrgType = albumType
  , mbrgFirstReleaseDate = firstReleaseDate
  , mbrgSecondaryTypes = []
  }

-- | Create a test MusicBrainz release with sensible defaults.
mkTestMBRelease
  :: Text  -- ^ Release MBID
  -> Text  -- ^ Album title
  -> Text  -- ^ Artist name
  -> Text  -- ^ Artist MBID
  -> Int   -- ^ Number of tracks
  -> MBRelease
mkTestMBRelease releaseMBID title artist artistMBID trackCount =
  let tracks = [ mkTestMBTrack i ("Track " <> show i) (i * 180000) | i <- [1..trackCount] ]
      -- Create a single medium with all tracks (for single-disc test releases)
      medium = MBMedium
        { mbMediumPosition = 1
        , mbMediumFormat = Just "Digital Media"
        , mbMediumTracks = tracks
        , mbMediumTrackCount = trackCount
        }
  in MBRelease
    { mbReleaseId = MBID releaseMBID
    , mbReleaseTitle = title
    , mbReleaseArtist = artist
    , mbReleaseArtistId = Just (MBID artistMBID)
    , mbReleaseArtists = [(MBID artistMBID, artist)]
    , mbReleaseDate = Just "2024-01-15"
    , mbReleaseYear = Just 2024
    , mbReleaseCountry = Just "US"
    , mbReleaseLabel = Just "Test Records"
    , mbReleaseCatalogNumber = Just "TEST-001"
    , mbReleaseBarcode = Nothing
    , mbReleaseGenres = ["Hip Hop", "Rap"]
    , mbReleaseMedia = [medium]
    , mbReleaseGroupId = Just (MBID $ releaseMBID <> "-rg")
    }

-- | Create a test MusicBrainz track.
mkTestMBTrack
  :: Int   -- ^ Track position
  -> Text  -- ^ Track title
  -> Int   -- ^ Duration in milliseconds
  -> MBTrack
mkTestMBTrack position title duration = MBTrack
  { mbTrackPosition = position
  , mbTrackTitle = title
  , mbTrackLength = Just duration
  , mbTrackRecordingId = MBID ("recording-" <> show position)
  , mbTrackArtist = Nothing  -- Use album artist
  }

-- | Create a test MusicBrainz release search response.
mkTestMBReleaseSearch :: [MBRelease] -> MBReleaseSearch
mkTestMBReleaseSearch releases = MBReleaseSearch
  { mbSearchReleases = releases
  , mbSearchCount = length releases
  }
