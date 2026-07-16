{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pure acquisition filter types and evaluation logic.
--
-- This module defines provider-specific filter types for each acquisition source:
-- - LibraryArtistsFilters: Filter artists from your library by ID and album criteria
-- - PitchforkFilters: Filter albums from Pitchfork by genre and score
-- - MetacriticFilters: Filter albums from Metacritic by genre and scores
module Skema.Domain.Acquisition
  ( -- * Provider-specific filter types
    LibraryArtistsFilters(..)
  , ReleaseStatusFilter(..)
  , PitchforkFilters(..)
  , PitchforkGenre(..)
  , pitchforkGenreToUrl
  , pitchforkGenreToDisplay
  , textToPitchforkGenre
  , MetacriticFilters(..)
  , MetacriticGenre(..)
  , metacriticGenreToUrl
  , metacriticGenreToDisplay
  , SourceFilters(..)
  , parseSourceFilters
    -- * Evaluation functions
  , shouldProcessArtistById
  , releaseDateIsUpcoming
  , matchesMetacriticFilters
  , matchesPitchforkFilters
  , evaluateLibraryArtistsAlbumFilters
  , takeNewEntries
  , metacriticGenresToScrape
  , matchesGenreFilter
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:?), eitherDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Data.Int ()
import Skema.Database.Types (AcquisitionSourceRecord(..), SourceType(..), CatalogAlbumRecord(..))
import Data.Time (UTCTime)

-- ============================================================================
-- Provider-Specific Filter Types
-- ============================================================================

-- | Pitchfork genres based on https://pitchfork.com/reviews/albums/
data PitchforkGenre
  = PFPop           -- ^ Pop (shows as Pop/R&B on homepage, links to /pop)
  | PFRock          -- ^ Rock
  | PFExperimental  -- ^ Experimental
  | PFElectronic    -- ^ Electronic
  | PFRap           -- ^ Rap
  | PFJazz          -- ^ Jazz
  | PFMetal         -- ^ Metal
  | PFFolkCountry   -- ^ Folk/Country (shows as Folk/Country, links to /folk)
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance ToJSON PitchforkGenre
instance FromJSON PitchforkGenre where
  parseJSON = withText "PitchforkGenre" $ \t -> case T.toLower t of
    "pop" -> pure PFPop
    "rock" -> pure PFRock
    "experimental" -> pure PFExperimental
    "electronic" -> pure PFElectronic
    "rap" -> pure PFRap
    "jazz" -> pure PFJazz
    "metal" -> pure PFMetal
    "folk" -> pure PFFolkCountry
    "folk/country" -> pure PFFolkCountry
    _ -> fail $ "Unknown Pitchfork genre: " <> T.unpack t

-- | Convert Pitchfork genre to URL path segment
pitchforkGenreToUrl :: PitchforkGenre -> Text
pitchforkGenreToUrl PFPop = "pop"
pitchforkGenreToUrl PFRock = "rock"
pitchforkGenreToUrl PFExperimental = "experimental"
pitchforkGenreToUrl PFElectronic = "electronic"
pitchforkGenreToUrl PFRap = "rap"
pitchforkGenreToUrl PFJazz = "jazz"
pitchforkGenreToUrl PFMetal = "metal"
pitchforkGenreToUrl PFFolkCountry = "folk"

-- | Convert Pitchfork genre to display name
pitchforkGenreToDisplay :: PitchforkGenre -> Text
pitchforkGenreToDisplay PFPop = "Pop"
pitchforkGenreToDisplay PFRock = "Rock"
pitchforkGenreToDisplay PFExperimental = "Experimental"
pitchforkGenreToDisplay PFElectronic = "Electronic"
pitchforkGenreToDisplay PFRap = "Rap"
pitchforkGenreToDisplay PFJazz = "Jazz"
pitchforkGenreToDisplay PFMetal = "Metal"
pitchforkGenreToDisplay PFFolkCountry = "Folk/Country"

-- | Convert display text from Pitchfork HTML to a PitchforkGenre.
textToPitchforkGenre :: Text -> Maybe PitchforkGenre
textToPitchforkGenre t = case T.toLower (T.strip t) of
  "pop"          -> Just PFPop
  "pop/r&b"      -> Just PFPop
  "rock"         -> Just PFRock
  "experimental" -> Just PFExperimental
  "electronic"   -> Just PFElectronic
  "rap"          -> Just PFRap
  "rap/hip-hop"  -> Just PFRap
  "jazz"         -> Just PFJazz
  "metal"        -> Just PFMetal
  "folk/country" -> Just PFFolkCountry
  "folk"         -> Just PFFolkCountry
  "country"      -> Just PFFolkCountry
  _              -> Nothing

-- | Metacritic genres from https://www.metacritic.com/browse/albums/genre/date/
-- These are the main genre categories available for browsing
data MetacriticGenre
  = MCPop           -- ^ Pop
  | MCRock          -- ^ Rock
  | MCAlternative   -- ^ Alternative & Indie
  | MCRap           -- ^ Rap
  | MCCountry       -- ^ Country
  | MCElectronic    -- ^ Electronic
  | MCRB            -- ^ R&B
  | MCJazz          -- ^ Jazz
  | MCFolk          -- ^ Folk
  | MCMetal         -- ^ Metal
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance ToJSON MetacriticGenre
instance FromJSON MetacriticGenre where
  parseJSON = withText "MetacriticGenre" $ \t -> case T.toLower t of
    "pop" -> pure MCPop
    "rock" -> pure MCRock
    "alternative" -> pure MCAlternative
    "alternative & indie" -> pure MCAlternative
    "rap" -> pure MCRap
    "country" -> pure MCCountry
    "electronic" -> pure MCElectronic
    "r&b" -> pure MCRB
    "rb" -> pure MCRB
    "jazz" -> pure MCJazz
    "folk" -> pure MCFolk
    "metal" -> pure MCMetal
    _ -> fail $ "Unknown Metacritic genre: " <> T.unpack t

-- | Convert Metacritic genre to URL path segment
metacriticGenreToUrl :: MetacriticGenre -> Text
metacriticGenreToUrl MCPop = "pop"
metacriticGenreToUrl MCRock = "rock"
metacriticGenreToUrl MCAlternative = "alternative"
metacriticGenreToUrl MCRap = "rap"
metacriticGenreToUrl MCCountry = "country"
metacriticGenreToUrl MCElectronic = "electronic"
metacriticGenreToUrl MCRB = "rb"
metacriticGenreToUrl MCJazz = "jazz"
metacriticGenreToUrl MCFolk = "folk"
metacriticGenreToUrl MCMetal = "metal"

-- | Convert Metacritic genre to display name
metacriticGenreToDisplay :: MetacriticGenre -> Text
metacriticGenreToDisplay MCPop = "Pop"
metacriticGenreToDisplay MCRock = "Rock"
metacriticGenreToDisplay MCAlternative = "Alternative & Indie"
metacriticGenreToDisplay MCRap = "Rap"
metacriticGenreToDisplay MCCountry = "Country"
metacriticGenreToDisplay MCElectronic = "Electronic"
metacriticGenreToDisplay MCRB = "R&B"
metacriticGenreToDisplay MCJazz = "Jazz"
metacriticGenreToDisplay MCFolk = "Folk"
metacriticGenreToDisplay MCMetal = "Metal"

-- | Release status filter options for Library Artists
data ReleaseStatusFilter
  = UpcomingOnly      -- ^ Only track upcoming/unreleased albums
  | ReleasedOnly      -- ^ Only track already released albums
  | UpcomingAndReleased  -- ^ Track both upcoming and released albums
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON ReleaseStatusFilter where
  toJSON UpcomingOnly = "upcoming"
  toJSON ReleasedOnly = "released"
  toJSON UpcomingAndReleased = "both"

instance FromJSON ReleaseStatusFilter where
  parseJSON = withText "ReleaseStatusFilter" $ \t -> case T.toLower t of
    "upcoming" -> pure UpcomingOnly
    "released" -> pure ReleasedOnly
    "both" -> pure UpcomingAndReleased
    _ -> fail $ "Unknown release status filter: " <> T.unpack t

-- | Filters for Library Artists source type.
-- Follows artists from your library based on artist IDs and album criteria.
data LibraryArtistsFilters = LibraryArtistsFilters
  { lafIncludeArtistIds :: Maybe [Int64]  -- ^ Include only these catalog artist IDs (null = all)
  , lafExcludeArtistIds :: Maybe [Int64]  -- ^ Exclude these catalog artist IDs
  , lafAlbumTypes :: Maybe [Text]         -- ^ Album types to track (Album, EP, Single, etc.)
  , lafReleaseStatus :: Maybe ReleaseStatusFilter  -- ^ Release status filter (default: UpcomingOnly)
  } deriving (Show, Eq, Generic)

instance FromJSON LibraryArtistsFilters where
  parseJSON = withObject "LibraryArtistsFilters" $ \o ->
    LibraryArtistsFilters
      <$> o .:? "include_artist_ids"
      <*> o .:? "exclude_artist_ids"
      <*> o .:? "album_types"
      <*> o .:? "release_status"

instance ToJSON LibraryArtistsFilters

-- | Filters for Pitchfork source type.
-- Scrapes Pitchfork reviews based on genre and score criteria.
data PitchforkFilters = PitchforkFilters
  { pfGenres :: Maybe [PitchforkGenre]  -- ^ Genres to include (null = all genres)
  , pfMinScore :: Maybe Double          -- ^ Minimum Pitchfork score (0-10 scale)
  } deriving (Show, Eq, Generic)

instance FromJSON PitchforkFilters where
  parseJSON = withObject "PitchforkFilters" $ \o ->
    PitchforkFilters
      <$> o .:? "genres"
      <*> o .:? "min_score"

instance ToJSON PitchforkFilters

-- | Filters for Metacritic source type.
-- Scrapes Metacritic reviews based on genre and score criteria.
data MetacriticFilters = MetacriticFilters
  { mcGenres :: Maybe [MetacriticGenre]  -- ^ Genres to include (null = all genres)
  , mcMinCriticScore :: Maybe Int        -- ^ Minimum critic score (0-100)
  , mcMinUserScore :: Maybe Double       -- ^ Minimum user score (0-10)
  } deriving (Show, Eq, Generic)

instance FromJSON MetacriticFilters where
  parseJSON = withObject "MetacriticFilters" $ \o ->
    MetacriticFilters
      <$> o .:? "genres"
      <*> o .:? "min_critic_score"
      <*> o .:? "min_user_score"

instance ToJSON MetacriticFilters

-- | Sum type for provider-specific filters.
-- Each source type has its own filter configuration.
data SourceFilters
  = LibraryArtistsSourceFilters LibraryArtistsFilters
  | PitchforkSourceFilters PitchforkFilters
  | MetacriticSourceFilters MetacriticFilters
  deriving (Show, Eq, Generic)

-- | Parse provider-specific filters from JSON text based on source type.
parseSourceFilters :: SourceType -> Maybe Text -> Maybe SourceFilters
parseSourceFilters _ Nothing = Nothing
parseSourceFilters st (Just jsonText) =
  case st of
    LibraryArtists ->
      case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
        Left _ -> Nothing
        Right filters -> Just (LibraryArtistsSourceFilters filters)
    Pitchfork ->
      case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
        Left _ -> Nothing
        Right filters -> Just (PitchforkSourceFilters filters)
    Metacritic ->
      case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
        Left _ -> Nothing
        Right filters -> Just (MetacriticSourceFilters filters)

-- | Check if we should process this artist by ID (for LibraryArtists source type).
-- Uses the new LibraryArtistsFilters which filters by catalog_artist.id instead of MBID.
shouldProcessArtistById :: AcquisitionSourceRecord -> Int64 -> Bool
shouldProcessArtistById source artistId = case sourceType source of
  LibraryArtists ->
    -- For library artists, check if artist passes ID-based filters
    case parseSourceFilters LibraryArtists (sourceFilters source) of
      Nothing -> True  -- No filters, process all library artists
      Just (LibraryArtistsSourceFilters filters) -> checkLibraryArtistFilters filters artistId
      _ -> False  -- Wrong filter type

  -- Non-LibraryArtists sources don't use artist matching
  _ -> False

-- | Check if an artist ID passes the LibraryArtistsFilters.
checkLibraryArtistFilters :: LibraryArtistsFilters -> Int64 -> Bool
checkLibraryArtistFilters filters artistId =
  let includePass = case lafIncludeArtistIds filters of
        Nothing -> True  -- No include list, pass all
        Just includeList -> artistId `elem` includeList
      excludePass = case lafExcludeArtistIds filters of
        Nothing -> True  -- No exclude list, pass all
        Just excludeList -> artistId `notElem` excludeList
  in includePass && excludePass

-- | Check if Metacritic critic and user scores satisfy the filter's thresholds.
--
-- A missing score against a configured threshold fails the filter
-- (we require the data we're filtering on). Missing thresholds pass.
matchesMetacriticFilters
  :: MetacriticFilters
  -> Maybe Int     -- ^ Critic score (0-100)
  -> Maybe Double  -- ^ User score (0-10)
  -> Bool
matchesMetacriticFilters MetacriticFilters{..} criticScore userScore =
  passesMin mcMinCriticScore criticScore && passesMin mcMinUserScore userScore
  where
    passesMin :: Ord a => Maybe a -> Maybe a -> Bool
    passesMin Nothing _ = True
    passesMin (Just _) Nothing = False
    passesMin (Just threshold) (Just value) = value >= threshold

-- | Check if a Pitchfork score satisfies the filter's threshold.
--
-- A missing score against a configured threshold fails the filter.
matchesPitchforkFilters :: PitchforkFilters -> Maybe Double -> Bool
matchesPitchforkFilters PitchforkFilters{..} score =
  case (pfMinScore, score) of
    (Nothing, _) -> True
    (Just _, Nothing) -> False
    (Just threshold, Just s) -> s >= threshold

-- | Whether a possibly partial MusicBrainz release date (YYYY, YYYY-MM, or
-- YYYY-MM-DD) is in the future. Partial dates are padded to the end of the
-- period they denote, so a year-only date counts as upcoming until that
-- year has fully passed instead of comparing lexicographically shorter
-- (and therefore always "released").
releaseDateIsUpcoming :: UTCTime -> Text -> Bool
releaseDateIsUpcoming now dateStr =
  let today = T.take 10 (show now)  -- Get YYYY-MM-DD part
      padded = case T.length dateStr of
        4 -> dateStr <> "-12-31"
        7 -> dateStr <> "-31"
        _ -> dateStr
  in padded > today

-- | Evaluate LibraryArtists filters against a catalog album to decide
-- whether it should be wanted. Combines an album-type check with a
-- release-status check (upcoming vs released), both derived purely from
-- the album record and the current time.
evaluateLibraryArtistsAlbumFilters :: LibraryArtistsFilters -> UTCTime -> CatalogAlbumRecord -> Bool
evaluateLibraryArtistsAlbumFilters filters now album =
  -- Check if album type matches filter
  let albumTypeMatch = case lafAlbumTypes filters of
        Nothing -> True  -- No album type filter specified, accept all types
        Just [] -> True  -- Empty list means include all types
        Just types -> case catalogAlbumType album of
          Just albumType -> albumType `elem` types
          Nothing -> True  -- If no type, include by default

      -- Check if release status matches filter
      isUpcoming = case catalogAlbumFirstReleaseDate album of
        Nothing -> False  -- No release date, not upcoming
        Just dateStr -> releaseDateIsUpcoming now dateStr

      releaseStatusMatch = case lafReleaseStatus filters of
        Nothing -> isUpcoming  -- Default: only upcoming albums (backward compatible)
        Just UpcomingOnly -> isUpcoming
        Just ReleasedOnly -> not isUpcoming
        Just UpcomingAndReleased -> True  -- Accept both

  -- Combine all checks (AND by default for album criteria)
  in albumTypeMatch && releaseStatusMatch

-- | Take entries from the front of a list until reaching the last-seen
-- entry (compared by key). 'Nothing' (first run) returns all entries.
-- Used to process only feed items newer than the last one we handled.
takeNewEntries :: Eq k => Maybe k -> (a -> k) -> [a] -> [a]
takeNewEntries Nothing _ entries = entries
takeNewEntries (Just lastKey) key entries = takeWhile (\e -> key e /= lastKey) entries

-- | The Metacritic genres to scrape for a source: the configured list when
-- present (including an explicit empty list), or a default set spanning the
-- main genres when none is configured.
metacriticGenresToScrape :: Maybe [MetacriticGenre] -> [MetacriticGenre]
metacriticGenresToScrape = fromMaybe defaultMetacriticGenres

defaultMetacriticGenres :: [MetacriticGenre]
defaultMetacriticGenres =
  [MCPop, MCRock, MCAlternative, MCRap, MCCountry, MCElectronic, MCRB, MCJazz, MCFolk, MCMetal]

-- | Does an item's genres satisfy a genre filter? An empty filter matches
-- everything; otherwise at least one of the item's genres must be wanted.
matchesGenreFilter :: Eq g => [g] -> [g] -> Bool
matchesGenreFilter [] _ = True
matchesGenreFilter wanted itemGenres = any (`elem` wanted) itemGenres
