{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Acquisition source filters and evaluation logic.
--
-- This module defines provider-specific filter types for each acquisition source:
-- - LibraryArtistsFilters: Filter artists from your library by ID and album criteria
-- - PitchforkFilters: Filter albums from Pitchfork by genre and score
-- - MetacriticFilters: Filter albums from Metacritic by genre and scores
module Skema.Services.Filters
  ( -- * Legacy types (for backward compatibility during migration)
    RuleFilters(..)
  , ArtistFilter(..)
  , ReviewFilter(..)
  , ReviewCriteria(..)
  , FilterOperator(..)
  , parseFilters
    -- * Provider-specific filter types
  , LibraryArtistsFilters(..)
  , ReleaseStatusFilter(..)
  , PitchforkFilters(..)
  , PitchforkGenre(..)
  , pitchforkGenreToUrl
  , pitchforkGenreToDisplay
  , MetacriticFilters(..)
  , MetacriticGenre(..)
  , metacriticGenreToUrl
  , metacriticGenreToDisplay
  , SourceFilters(..)
  , parseSourceFilters
    -- * Evaluation functions
  , shouldProcessArtist
  , shouldProcessArtistById
  , shouldIncludeReleaseGroup
  , mbReleaseGroupIsUpcoming
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:), (.:?), (.!=), eitherDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Data.Int ()
import Skema.Database.Types (AcquisitionSourceRecord(..), SourceType(..))
import Skema.MusicBrainz.Types (MBReleaseGroup(..))
import Data.Time (UTCTime)

-- | Logical operator for combining filter criteria.
data FilterOperator = AND | OR
  deriving (Show, Eq, Generic)

instance FromJSON FilterOperator where
  parseJSON = withObject "FilterOperator" $ \o -> do
    op <- o .: "operator"
    case op :: Text of
      "AND" -> pure AND
      "OR" -> pure OR
      _ -> fail "Invalid operator, must be 'AND' or 'OR'"

instance ToJSON FilterOperator

-- | Artist filter criteria.
data ArtistFilter = ArtistFilter
  { afInclude :: Maybe [Text]  -- ^ Include only these artist MBIDs
  , afExclude :: Maybe [Text]  -- ^ Exclude these artist MBIDs
  } deriving (Show, Eq, Generic)

instance FromJSON ArtistFilter where
  parseJSON = withObject "ArtistFilter" $ \o ->
    ArtistFilter
      <$> o .:? "include"
      <*> o .:? "exclude"

instance ToJSON ArtistFilter

-- | Review score criteria for a single platform.
data ReviewCriteria = ReviewCriteria
  { rcMinScore :: Maybe Double  -- ^ Minimum score threshold
  , rcMaxScore :: Maybe Double  -- ^ Maximum score threshold
  } deriving (Show, Eq, Generic)

instance FromJSON ReviewCriteria where
  parseJSON = withObject "ReviewCriteria" $ \o ->
    ReviewCriteria
      <$> o .:? "min_score"
      <*> o .:? "max_score"

instance ToJSON ReviewCriteria

-- | Review filter criteria.
data ReviewFilter = ReviewFilter
  { rfMetacritic :: Maybe ReviewCriteria  -- ^ Metacritic score filter
  , rfPitchfork :: Maybe ReviewCriteria   -- ^ Pitchfork score filter
  , rfOperator :: FilterOperator          -- ^ How to combine scores (AND/OR)
  } deriving (Show, Eq, Generic)

instance FromJSON ReviewFilter where
  parseJSON = withObject "ReviewFilter" $ \o ->
    ReviewFilter
      <$> o .:? "metacritic"
      <*> o .:? "pitchfork"
      <*> o .:? "operator" .!= OR

instance ToJSON ReviewFilter

-- | Complete filter configuration for an acquisition rule.
data RuleFilters = RuleFilters
  { filtersArtists :: Maybe ArtistFilter      -- ^ Artist inclusion/exclusion
  , filtersReviews :: Maybe ReviewFilter      -- ^ Review score filters
  , filtersGenres :: Maybe [Text]             -- ^ Genre filters
  , filtersAlbumTypes :: Maybe [Text]         -- ^ Album types (Album, EP, Single, etc.)
  , filtersReleaseStatus :: Maybe [Text]      -- ^ Release status (upcoming, released)
  , filtersOperator :: FilterOperator         -- ^ Top-level operator for combining filters
  } deriving (Show, Eq, Generic)

instance FromJSON RuleFilters where
  parseJSON = withObject "RuleFilters" $ \o ->
    RuleFilters
      <$> o .:? "artists"
      <*> o .:? "reviews"
      <*> o .:? "genres"
      <*> o .:? "album_types"
      <*> o .:? "release_status"
      <*> o .:? "operator" .!= AND

instance ToJSON RuleFilters

-- | Parse filters from JSON text (legacy).
parseFilters :: Maybe Text -> Maybe RuleFilters
parseFilters Nothing = Nothing
parseFilters (Just jsonText) =
  case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
    Left _ -> Nothing
    Right filters -> Just filters

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
parseSourceFilters sourceType (Just jsonText) =
  case sourceType of
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

-- | Check if we should process this artist based on the source and filters.
shouldProcessArtist :: AcquisitionSourceRecord -> Text -> Bool
shouldProcessArtist source artistMBID = case sourceType source of
  LibraryArtists ->
    -- For library artists, check if artist passes filters
    case parseFilters (sourceFilters source) of
      Nothing -> True  -- No filters, process all library artists
      Just filters -> checkArtistFilters filters artistMBID

  Metacritic ->
    -- Metacritic sources don't use artist matching
    False

  Pitchfork ->
    -- Pitchfork sources don't use artist matching
    False

-- | Check if an artist passes the artist filters (legacy MBID-based).
checkArtistFilters :: RuleFilters -> Text -> Bool
checkArtistFilters filters artistMBID =
  case filtersArtists filters of
    Nothing -> True  -- No artist filter, pass all
    Just af ->
      let includePass = case afInclude af of
            Nothing -> True  -- No include list, pass all
            Just includeList -> artistMBID `elem` includeList
          excludePass = case afExclude af of
            Nothing -> True  -- No exclude list, pass all
            Just excludeList -> artistMBID `notElem` excludeList
      in includePass && excludePass

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

-- | Check if a release group should be included based on filters.
shouldIncludeReleaseGroup
  :: AcquisitionSourceRecord
  -> UTCTime
  -> MBReleaseGroup
  -> Maybe Double  -- ^ Metacritic score (if available)
  -> Maybe Double  -- ^ Pitchfork score (if available)
  -> Bool
shouldIncludeReleaseGroup source now rg metacriticScore pitchforkScore =
  case parseFilters (sourceFilters source) of
    Nothing -> False  -- No filters configured, don't mark as wanted
    Just filters -> evaluateFilters filters
  where
    isUpcoming = mbReleaseGroupIsUpcoming now rg

    -- Evaluate all filters based on the operator
    evaluateFilters filters = case filtersOperator filters of
      AND -> checkAlbumType filters && checkReleaseStatus filters && checkReviews filters && checkGenres filters
      OR -> checkAlbumType filters || checkReleaseStatus filters || checkReviews filters || checkGenres filters

    -- Check if album type matches filter
    checkAlbumType filters = case filtersAlbumTypes filters of
      Nothing -> True  -- No album type filter specified, accept all types
      Just types -> case mbrgType rg of
        Just rgType -> rgType `elem` types
        Nothing -> True  -- If no type, include by default

    -- Check if release status matches filter
    checkReleaseStatus filters = case filtersReleaseStatus filters of
      Nothing -> True  -- No release status filter specified, accept all statuses
      Just statuses ->
        let isReleased = not isUpcoming
        in ("upcoming" `elem` statuses && isUpcoming) ||
           ("released" `elem` statuses && isReleased)

    -- Check if reviews meet criteria
    checkReviews filters = case filtersReviews filters of
      Nothing -> True  -- No review filter
      Just rf ->
        let metacriticPass = case rfMetacritic rf of
              Nothing -> True
              Just criteria -> checkReviewCriteria criteria metacriticScore
            pitchforkPass = case rfPitchfork rf of
              Nothing -> True
              Just criteria -> checkReviewCriteria criteria pitchforkScore
        in case rfOperator rf of
             AND -> metacriticPass && pitchforkPass
             OR -> metacriticPass || pitchforkPass

    -- Check if genres match (Note: MusicBrainz doesn't provide genre in release groups directly)
    -- This would need to be fetched separately or we'd need to enhance the MB client
    checkGenres filters = case filtersGenres filters of
      Nothing -> True  -- No genre filter
      Just _genres -> True  -- TODO: Implement genre checking when we have genre data

-- | Check if a score meets review criteria.
checkReviewCriteria :: ReviewCriteria -> Maybe Double -> Bool
checkReviewCriteria _criteria Nothing = False  -- No score available
checkReviewCriteria ReviewCriteria{..} (Just score) =
  let minPass = case rcMinScore of
        Nothing -> True
        Just minScore -> score >= minScore
      maxPass = case rcMaxScore of
        Nothing -> True
        Just maxScore -> score <= maxScore
  in minPass && maxPass

-- | Check if a release group is upcoming (release date in the future).
mbReleaseGroupIsUpcoming :: UTCTime -> MBReleaseGroup -> Bool
mbReleaseGroupIsUpcoming _now rg = case mbrgFirstReleaseDate rg of
  Nothing -> False  -- No release date, not upcoming
  Just dateStr ->
    -- Parse date string (YYYY-MM-DD format)
    -- For simplicity, compare as strings (works for ISO 8601 dates)
    let today = T.take 10 (show _now)  -- Get YYYY-MM-DD part
    in dateStr > today
