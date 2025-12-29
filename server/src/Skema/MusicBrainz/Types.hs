{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | MusicBrainz types and API client.
module Skema.MusicBrainz.Types
  ( -- * MusicBrainz IDs
    MBID (..)
  , ReleaseMBID
  , RecordingMBID
  , ReleaseGroupMBID
  , ArtistMBID
    -- * MusicBrainz Entities
  , MBRelease (..)
  , mbReleaseTracks
  , MBMedium (..)
  , MBTrack (..)
  , MBRecording (..)
  , MBReleaseSearch (..)
  , MBRecordingSearch (..)
  , MBArtistSearch (..)
  , MBReleaseGroupSearch (..)
  , MBReleaseGroup (..)
  , MBArtist (..)
  , MBArtistSearchResult (..)
  , MBReleaseGroupSearchResult (..)
    -- * Track Matching
  , FileGroup (..)
  , TrackMatch (..)
  , ReleaseMatch (..)
  , IdentificationResult (..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import System.OsPath (OsPath)
import Monatone.Metadata (Metadata)

-- | MusicBrainz ID (UUID).
newtype MBID = MBID { unMBID :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MBID where
  toJSON = toJSON . unMBID

instance FromJSON MBID where
  parseJSON = fmap MBID . parseJSON

type ReleaseMBID = MBID
type RecordingMBID = MBID
type ReleaseGroupMBID = MBID
type ArtistMBID = MBID

-- | MusicBrainz Release (album).
data MBRelease = MBRelease
  { mbReleaseId :: ReleaseMBID
  , mbReleaseTitle :: Text
  , mbReleaseArtist :: Text
    -- ^ Full artist credit string (e.g., "JAY-Z & Kanye West")
  , mbReleaseArtistId :: Maybe ArtistMBID
    -- ^ Primary artist ID (first in credit)
  , mbReleaseArtists :: [(ArtistMBID, Text)]
    -- ^ All artists in the credit (for collaborative albums)
  , mbReleaseDate :: Maybe Text
  , mbReleaseYear :: Maybe Int  -- Extracted from date
  , mbReleaseCountry :: Maybe Text
  , mbReleaseLabel :: Maybe Text
  , mbReleaseCatalogNumber :: Maybe Text
  , mbReleaseBarcode :: Maybe Text
  , mbReleaseGenres :: [Text]  -- List of genre names
  , mbReleaseMedia :: [MBMedium]
    -- ^ Media (discs, vinyl sides, etc.) each containing tracks
  , mbReleaseGroupId :: Maybe ReleaseGroupMBID
  } deriving (Show, Eq, Generic)

-- | Helper function to get all tracks from all media (flattened)
-- Provided for backwards compatibility with existing code
mbReleaseTracks :: MBRelease -> [MBTrack]
mbReleaseTracks = concatMap mbMediumTracks . mbReleaseMedia

instance FromJSON MBRelease where
  parseJSON = withObject "MBRelease" $ \o -> do
    mbReleaseId <- o .: "id"
    mbReleaseTitle <- o .: "title"
    -- Parse artist-credit array: each element has "name", "joinphrase", and "artist" object
    -- Example: [{"name": "JAY-Z", "joinphrase": " & ", "artist": {"id": "..."}},
    --           {"name": "Kanye West", "joinphrase": "", "artist": {"id": "..."}}]
    -- Should produce: "JAY-Z & Kanye West"
    (mbReleaseArtist, mbReleaseArtistId, mbReleaseArtists) <- o .:? "artist-credit" >>= \case
      Just (Array v) -> do
        let parseArtistCredit (Object ac) = do
              name <- ac .: "name"
              joinphrase <- ac .:? "joinphrase" .!= ""
              artistId <- ac .:? "artist" >>= \case
                Just (Object artist) -> artist .:? "id"
                _ -> pure Nothing
              pure (name :: Text, joinphrase :: Text, artistId :: Maybe ArtistMBID)
            parseArtistCredit _ = pure ("", "", Nothing)
        credits <- mapM parseArtistCredit (toList v)
        let artistName = T.concat [name <> joinphrase | (name, joinphrase, _) <- credits]
        let primaryArtistId = case viaNonEmpty head credits of
              Just (_, _, aid) -> aid
              Nothing -> Nothing
        -- Collect all artists with IDs (filter out features without IDs)
        let allArtists = [(aid, name) | (name, _, Just aid) <- credits]
        pure (artistName, primaryArtistId, allArtists)
      _ -> pure ("", Nothing, [])
    mbReleaseDate <- o .:? "date"
    -- Extract year from date (YYYY-MM-DD -> YYYY)
    let mbReleaseYear = mbReleaseDate >>= \d ->
          case T.splitOn "-" d of
            (y:_) -> readMaybe (toString y)
            _ -> Nothing
    mbReleaseCountry <- o .:? "country"
    -- Extract label and catalog number from label-info array
    (mbReleaseLabel, mbReleaseCatalogNumber) <- o .:? "label-info" >>= \case
      Just (Array v) -> case viaNonEmpty head (toList v) of
        Just (Object li) -> do
          label <- li .:? "label" >>= \case
            Just (Object l) -> l .:? "name"
            _ -> pure Nothing
          catalogNum <- li .:? "catalog-number"
          pure (label, catalogNum)
        _ -> pure (Nothing, Nothing)
      _ -> pure (Nothing, Nothing)
    mbReleaseBarcode <- o .:? "barcode"
    -- Extract genres from genres array
    mbReleaseGenres <- o .:? "genres" >>= \case
      Just (Array v) -> catMaybes <$> forM (toList v) (\case
        Object g -> g .:? "name"
        _ -> pure Nothing)
      _ -> pure []
    -- Parse media (discs, vinyl sides, etc.)
    mbReleaseMedia <- o .:? "media" .!= []
    mbReleaseGroupId <- o .:? "release-group" >>= \case
      Just (Object rg) -> rg .:? "id"
      _ -> pure Nothing
    pure MBRelease{..}

-- Custom ToJSON to match FromJSON format (for round-trip serialization)
instance ToJSON MBRelease where
  toJSON release =
    -- Use mbReleaseArtist for the full artist string (with join phrases)
    -- and mbReleaseArtistId for the primary artist ID
    let artistCredits = toJSON [object
          [ "name" .= mbReleaseArtist release
          , "joinphrase" .= ("" :: Text)
          , "artist" .= maybe Null (\aid -> object ["id" .= aid]) (mbReleaseArtistId release)
          ]]
        mediaArray = toJSON (mbReleaseMedia release)
        labelInfo = case (mbReleaseLabel release, mbReleaseCatalogNumber release) of
          (Nothing, Nothing) -> Null
          (label, catNum) -> toJSON [object
            [ "label" .= maybe Null (\l -> object ["name" .= l]) label
            , "catalog-number" .= catNum
            ]]
        genresArray = toJSON $ map (\g -> object ["name" .= g]) (mbReleaseGenres release)
        releaseGroupObj = case mbReleaseGroupId release of
          Nothing -> Null
          Just rgid -> object ["id" .= rgid]
    in object
      [ "id" .= mbReleaseId release
      , "title" .= mbReleaseTitle release
      , "artist-credit" .= artistCredits
      , "date" .= mbReleaseDate release
      , "country" .= mbReleaseCountry release
      , "label-info" .= labelInfo
      , "barcode" .= mbReleaseBarcode release
      , "genres" .= genresArray
      , "media" .= mediaArray
      , "release-group" .= releaseGroupObj
      ]

-- | MusicBrainz Medium (disc, vinyl side, cassette side, etc.)
data MBMedium = MBMedium
  { mbMediumPosition :: Int
    -- ^ Position of this medium in the release (1-based)
  , mbMediumFormat :: Maybe Text
    -- ^ Format type: "CD", "Digital Media", "Vinyl", "Cassette", etc.
  , mbMediumTracks :: [MBTrack]
    -- ^ Tracks on this medium
  , mbMediumTrackCount :: Int
    -- ^ Number of tracks on this medium
  } deriving (Show, Eq, Generic)

instance FromJSON MBMedium where
  parseJSON = withObject "MBMedium" $ \o -> do
    mbMediumPosition <- o .:? "position" .!= 1
    mbMediumFormat <- o .:? "format"
    mbMediumTracks <- o .:? "tracks" .!= []
    let mbMediumTrackCount = length mbMediumTracks
    pure MBMedium{..}

instance ToJSON MBMedium where
  toJSON medium = object
    [ "position" .= mbMediumPosition medium
    , "format" .= mbMediumFormat medium
    , "tracks" .= mbMediumTracks medium
    , "track-count" .= mbMediumTrackCount medium
    ]

-- | MusicBrainz Track (track on a release).
data MBTrack = MBTrack
  { mbTrackPosition :: Int
  , mbTrackTitle :: Text
  , mbTrackLength :: Maybe Int  -- Duration in milliseconds
  , mbTrackRecordingId :: RecordingMBID
  , mbTrackArtist :: Maybe Text  -- Track-level artist (can differ from album artist)
  } deriving (Show, Eq, Generic)

instance FromJSON MBTrack where
  parseJSON = withObject "MBTrack" $ \o -> do
    mbTrackPosition <- o .: "position" >>= parseJSON
    mbTrackTitle <- o .: "title"
    mbTrackLength <- o .:? "length"
    mbTrackRecordingId <- o .: "recording" >>= \case
      Object rec -> rec .: "id"
      _ -> fail "Expected recording object"
    -- Parse artist-credit array: each element has "name" and "joinphrase"
    -- Example: [{"name": "JAY-Z", "joinphrase": " feat. "}, {"name": "Gloria Carter", "joinphrase": ""}]
    -- Should produce: "JAY-Z feat. Gloria Carter"
    mbTrackArtist <- o .:? "artist-credit" >>= \case
      Just (Array v) -> do
        let parseArtistCredit (Object ac) = do
              name <- ac .: "name"
              joinphrase <- ac .:? "joinphrase" .!= ""
              pure (name :: Text, joinphrase :: Text)
            parseArtistCredit _ = pure ("", "")
        credits <- mapM parseArtistCredit (toList v)
        let artist = T.concat [name <> joinphrase | (name, joinphrase) <- credits]
        pure $ if T.null artist then Nothing else Just artist
      _ -> pure Nothing
    pure MBTrack{..}

-- Custom ToJSON to match FromJSON format
instance ToJSON MBTrack where
  toJSON track =
    let artistCredit = case mbTrackArtist track of
          Nothing -> Null
          Just artist -> toJSON [object ["name" .= artist, "joinphrase" .= ("" :: Text)]]
    in object
      [ "position" .= mbTrackPosition track
      , "title" .= mbTrackTitle track
      , "length" .= mbTrackLength track
      , "recording" .= object ["id" .= mbTrackRecordingId track]
      , "artist-credit" .= artistCredit
      ]

-- | MusicBrainz Recording (master recording).
data MBRecording = MBRecording
  { mbRecordingId :: RecordingMBID
  , mbRecordingTitle :: Text
  , mbRecordingLength :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON MBRecording where
  parseJSON = withObject "MBRecording" $ \o -> do
    mbRecordingId <- o .: "id"
    mbRecordingTitle <- o .: "title"
    mbRecordingLength <- o .:? "length"
    pure MBRecording{..}

-- | MusicBrainz release search result.
data MBReleaseSearch = MBReleaseSearch
  { mbSearchReleases :: [MBRelease]
  , mbSearchCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON MBReleaseSearch where
  parseJSON = withObject "MBReleaseSearch" $ \o -> do
    mbSearchReleases <- o .: "releases"
    mbSearchCount <- o .: "count"
    pure MBReleaseSearch{..}

-- | MusicBrainz recording search result.
data MBRecordingSearch = MBRecordingSearch
  { mbSearchRecordings :: [MBRecording]
  , mbSearchRecordingCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON MBRecordingSearch where
  parseJSON = withObject "MBRecordingSearch" $ \o -> do
    mbSearchRecordings <- o .: "recordings"
    mbSearchRecordingCount <- o .: "count"
    pure MBRecordingSearch{..}

-- | MusicBrainz Release Group (represents an album across all its releases).
data MBReleaseGroup = MBReleaseGroup
  { mbrgId :: ReleaseGroupMBID
  , mbrgTitle :: Text
  , mbrgType :: Maybe Text  -- Primary type: e.g., "Album", "EP", "Single"
  , mbrgSecondaryTypes :: [Text]  -- Secondary types: e.g., "Live", "Compilation"
  , mbrgFirstReleaseDate :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON MBReleaseGroup where
  parseJSON = withObject "MBReleaseGroup" $ \o -> do
    mbrgId <- o .: "id"
    mbrgTitle <- o .: "title"
    mbrgType <- o .:? "primary-type"
    mbrgSecondaryTypes <- o .:? "secondary-types" .!= []
    mbrgFirstReleaseDate <- o .:? "first-release-date"
    pure MBReleaseGroup{..}

-- | MusicBrainz Artist with release groups.
data MBArtist = MBArtist
  { mbArtistId :: ArtistMBID
  , mbArtistName :: Text
  , mbArtistReleaseGroups :: [MBReleaseGroup]
  } deriving (Show, Eq, Generic)

instance FromJSON MBArtist where
  parseJSON = withObject "MBArtist" $ \o -> do
    mbArtistId <- o .: "id"
    mbArtistName <- o .: "name"
    mbArtistReleaseGroups <- o .:? "release-groups" .!= []
    pure MBArtist{..}

-- | MusicBrainz Artist search result (simplified).
data MBArtistSearchResult = MBArtistSearchResult
  { mbasArtistId :: ArtistMBID
  , mbasArtistName :: Text
  , mbasArtistType :: Maybe Text  -- e.g., "Person", "Group"
  , mbasScore :: Int  -- Search score (0-100)
  } deriving (Show, Eq, Generic)

instance FromJSON MBArtistSearchResult where
  parseJSON = withObject "MBArtistSearchResult" $ \o -> do
    mbasArtistId <- o .: "id"
    mbasArtistName <- o .: "name"
    mbasArtistType <- o .:? "type"
    mbasScore <- o .: "score"
    pure MBArtistSearchResult{..}

instance ToJSON MBArtistSearchResult where
  toJSON result = object
    [ "id" .= mbasArtistId result
    , "name" .= mbasArtistName result
    , "type" .= mbasArtistType result
    , "score" .= mbasScore result
    ]

-- | MusicBrainz artist search results.
data MBArtistSearch = MBArtistSearch
  { mbasArtists :: [MBArtistSearchResult]
  , mbasCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON MBArtistSearch where
  parseJSON = withObject "MBArtistSearch" $ \o -> do
    mbasArtists <- o .: "artists"
    mbasCount <- o .: "count"
    pure MBArtistSearch{..}

-- | MusicBrainz Release Group search result (simplified).
data MBReleaseGroupSearchResult = MBReleaseGroupSearchResult
  { mbrgsReleaseGroupId :: ReleaseGroupMBID
  , mbrgsTitle :: Text
  , mbrgsArtistName :: Text
  , mbrgsArtistId :: Maybe ArtistMBID
  , mbrgsType :: Maybe Text  -- e.g., "Album", "EP", "Single"
  , mbrgsFirstReleaseDate :: Maybe Text
  , mbrgsScore :: Int  -- Search score (0-100)
  } deriving (Show, Eq, Generic)

instance FromJSON MBReleaseGroupSearchResult where
  parseJSON = withObject "MBReleaseGroupSearchResult" $ \o -> do
    mbrgsReleaseGroupId <- o .: "id"
    mbrgsTitle <- o .: "title"
    -- Parse artist-credit array like in MBRelease
    mbrgsArtistName <- o .:? "artist-credit" >>= \case
      Just (Array v) -> do
        let parseArtistCredit (Object ac) = do
              name <- ac .: "name"
              joinphrase <- ac .:? "joinphrase" .!= ""
              pure (name :: Text, joinphrase :: Text)
            parseArtistCredit _ = pure ("", "")
        credits <- mapM parseArtistCredit (toList v)
        let artistName = T.concat [name <> joinphrase | (name, joinphrase) <- credits]
        pure artistName
      _ -> pure ""
    -- Try to get artist ID from first credit
    mbrgsArtistId <- o .:? "artist-credit" >>= \case
      Just (Array v) -> case viaNonEmpty head (toList v) of
        Just (Object ac) -> ac .:? "artist" >>= \case
          Just (Object artist) -> artist .:? "id"
          _ -> pure Nothing
        _ -> pure Nothing
      _ -> pure Nothing
    mbrgsType <- o .:? "primary-type"
    mbrgsFirstReleaseDate <- o .:? "first-release-date"
    mbrgsScore <- o .: "score"
    pure MBReleaseGroupSearchResult{..}

instance ToJSON MBReleaseGroupSearchResult where
  toJSON result = object
    [ "id" .= mbrgsReleaseGroupId result
    , "title" .= mbrgsTitle result
    , "artist_name" .= mbrgsArtistName result
    , "artist_id" .= mbrgsArtistId result
    , "type" .= mbrgsType result
    , "first_release_date" .= mbrgsFirstReleaseDate result
    , "score" .= mbrgsScore result
    ]

-- | MusicBrainz release group search results.
data MBReleaseGroupSearch = MBReleaseGroupSearch
  { mbrgsReleaseGroups :: [MBReleaseGroupSearchResult]
  , mbrgsCount :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON MBReleaseGroupSearch where
  parseJSON = withObject "MBReleaseGroupSearch" $ \o -> do
    mbrgsReleaseGroups <- o .: "release-groups"
    mbrgsCount <- o .: "count"
    pure MBReleaseGroupSearch{..}

-- | A group of files that likely belong to the same release.
data FileGroup = FileGroup
  { fgDirectory :: OsPath
  , fgAlbum :: Maybe Text
  , fgArtist :: Maybe Text
  , fgReleaseId :: Maybe ReleaseMBID  -- From existing tags
  , fgReleaseGroupId :: Maybe ReleaseGroupMBID  -- From existing tags
  , fgLabel :: Maybe Text
  , fgCatalogNumber :: Maybe Text
  , fgBarcode :: Maybe Text
  , fgCountry :: Maybe Text
  , fgDate :: Maybe Text
  , fgFiles :: [(OsPath, Metadata)]  -- File path and parsed metadata
  } deriving (Show, Eq, Generic)

-- | Result of matching a track to a MusicBrainz track.
data TrackMatch = TrackMatch
  { tmFilePath :: OsPath
  , tmTrack :: MBTrack
  , tmCost :: Double  -- Lower is better
  , tmConfidence :: Double  -- 0.0 to 1.0
  } deriving (Show, Eq, Generic)

-- | Result of matching a file group to a MusicBrainz release.
data ReleaseMatch = ReleaseMatch
  { rmFileGroup :: FileGroup
  , rmRelease :: MBRelease
  , rmTrackMatches :: [TrackMatch]
  , rmTotalCost :: Double  -- Lower is better
  , rmConfidence :: Double  -- 0.0 to 1.0, based on how many tracks matched well
  , rmCandidates :: [MBRelease]  -- All candidate releases found (for user selection)
  } deriving (Show, Eq, Generic)

-- | Result of identification: either a match above threshold, or just candidates
data IdentificationResult = IdentificationResult
  { irMatch :: Maybe ReleaseMatch  -- Best match if above confidence threshold
  , irCandidates :: [(MBRelease, Double)]  -- All candidates with their confidence scores
  } deriving (Show, Eq, Generic)
