{-# LANGUAGE OverloadedStrings #-}

-- | Pure identification domain logic.
--
-- This module contains all pure business logic for MusicBrainz identification.
-- It has no IO operations and is fully testable.
module Skema.Domain.Identification
  ( -- * Configuration
    IdentifyConfig(..)
  , defaultIdentifyConfig
    -- * Search query building
  , buildSearchQuery
  , normalizeSearchText
  , escapeQuery
    -- * Release ranking and selection
  , rankReleaseCandidates
  , selectBestMatch
    -- * Match computation
  , computeReleaseMatch
  , shouldRetryIdentification
  ) where

import Skema.MusicBrainz.Types
import Skema.MusicBrainz.Matching
import Data.List ()
import qualified Data.Text as T
import Data.Time (UTCTime, NominalDiffTime, addUTCTime)

-- | Configuration for identification process
data IdentifyConfig = IdentifyConfig
  { cfgMaxCandidates :: Int
    -- ^ Maximum number of release candidates to consider (default: 5)
  , cfgMinConfidence :: Double
    -- ^ Minimum confidence threshold for accepting a match (0.0-1.0, default: 0.35)
  , cfgSearchLimit :: Int
    -- ^ Maximum number of releases to fetch from MusicBrainz search (default: 20)
  , cfgNormalizeFeaturing :: Bool
    -- ^ Normalize "featuring" join phrases in artist credits (default: False)
  , cfgNormalizeFeaturingTo :: Text
    -- ^ What to normalize "featuring" to (e.g., "feat.", default: "feat.")
  , cfgRetryIntervalHours :: NominalDiffTime
    -- ^ Hours to wait before retrying failed identification (default: 24)
  } deriving (Show, Eq)

-- | Default identification configuration
defaultIdentifyConfig :: IdentifyConfig
defaultIdentifyConfig = IdentifyConfig
  { cfgMaxCandidates = 5
  , cfgMinConfidence = 0.35
  , cfgSearchLimit = 20
  , cfgNormalizeFeaturing = False
  , cfgNormalizeFeaturingTo = "feat."
  , cfgRetryIntervalHours = 24
  }

-- | Build a MusicBrainz search query from file group metadata.
--
-- Uses album + artist as core query. Other fields are only included if non-empty
-- to prevent over-constraining the search.
--
-- This is a pure function with no side effects.
buildSearchQuery :: FileGroup -> Text
buildSearchQuery fg =
  let -- Core query: album and artist (required)
      coreParts = catMaybes
        [ (\a -> "release:\"" <> escapeQuery (normalizeSearchText a) <> "\"") <$> fgAlbum fg
        , (\a -> "artist:\"" <> escapeQuery (normalizeSearchText a) <> "\"") <$> fgArtist fg
        ]

      -- Optional parts: only include if non-empty and non-trivial
      -- Barcode is highly specific, so include it
      optionalParts = catMaybes
        [ (\b -> "barcode:" <> b) <$> (fgBarcode fg >>= \b -> if T.null b then Nothing else Just b)
        ]

      allParts = coreParts <> optionalParts
  in T.intercalate " AND " allParts

-- | Normalize text for MusicBrainz search queries.
--
-- Converts Unicode punctuation variants to ASCII equivalents.
-- This is a pure transformation with no side effects.
normalizeSearchText :: Text -> Text
normalizeSearchText =
  -- Normalize various Unicode hyphens/dashes to ASCII hyphen-minus
  T.replace "\x2010" "-"  -- HYPHEN
  . T.replace "\x2011" "-"  -- NON-BREAKING HYPHEN
  . T.replace "\x2012" "-"  -- FIGURE DASH
  . T.replace "\x2013" "-"  -- EN DASH
  . T.replace "\x2014" "-"  -- EM DASH
  . T.replace "\x2015" "-"  -- HORIZONTAL BAR
  -- Normalize various Unicode quotes to ASCII
  . T.replace "\x2018" "'"  -- LEFT SINGLE QUOTATION MARK
  . T.replace "\x2019" "'"  -- RIGHT SINGLE QUOTATION MARK
  . T.replace "\x201C" "\""  -- LEFT DOUBLE QUOTATION MARK
  . T.replace "\x201D" "\""  -- RIGHT DOUBLE QUOTATION MARK

-- | Escape special characters in Lucene query syntax.
--
-- Pure string transformation with no side effects.
escapeQuery :: Text -> Text
escapeQuery = T.replace "\"" "\\\"" . T.replace ":" "\\:"

-- | Rank release candidates by quality of match.
--
-- This is pure business logic that filters and scores candidates.
-- Returns candidates sorted by cost (best first), limited to maxCandidates.
--
-- This is a pure function with no side effects.
rankReleaseCandidates :: Int -> FileGroup -> [MBRelease] -> [(MBRelease, Double)]
rankReleaseCandidates maxCandidates fg candidates =
  take maxCandidates $ matchReleases fg candidates

-- | Compute a detailed release match with track matching.
--
-- This performs the full matching algorithm:
-- 1. Matches tracks using Hungarian algorithm
-- 2. Computes combined cost (30% release metadata, 70% track matching)
-- 3. Calculates confidence score
--
-- This is a pure function with no side effects.
computeReleaseMatch :: FileGroup
                    -> [MBRelease]  -- ^ All candidates for context
                    -> (MBRelease, Double)  -- ^ Candidate to match with its release cost
                    -> ReleaseMatch
computeReleaseMatch fg allCandidates (release, releaseCostValue) =
  let trackMatches = matchTracksToRelease fg release
      matchedCount = length trackMatches
      totalTracks = length (fgFiles fg)

      -- Handle case where no tracks could be matched (empty release or parse error)
      totalCost = if null trackMatches
                  then 1.0  -- Maximum cost if no tracks matched
                  else sum (map tmCost trackMatches) / fromIntegral (length trackMatches)

      -- Combine release cost with track matching results
      -- Weight: 30% release metadata, 70% track matching
      combinedCost = releaseCostValue * (0.3 :: Double) + totalCost * (0.7 :: Double)

      -- Confidence based on cost and match completeness
      confidence = if totalTracks == 0
                   then 0.0
                   else (1.0 - combinedCost) * (fromIntegral matchedCount / fromIntegral totalTracks :: Double)

  in ReleaseMatch
    { rmFileGroup = fg
    , rmRelease = release
    , rmTrackMatches = trackMatches
    , rmTotalCost = combinedCost
    , rmConfidence = confidence
    , rmCandidates = allCandidates
    }

-- | Select the best match from a list of candidates.
--
-- Returns Just match if the best candidate meets the minimum confidence threshold,
-- otherwise Nothing.
--
-- This is a pure decision function with no side effects.
selectBestMatch :: Double -> [ReleaseMatch] -> Maybe ReleaseMatch
selectBestMatch minConfidence matches =
  case viaNonEmpty head $ sortOn (negate . rmConfidence) matches of
    Just match | rmConfidence match >= minConfidence -> Just match
    _ -> Nothing

-- | Determine if a cluster should be retried for identification.
--
-- Pure business logic for deciding whether to retry identification based on:
-- - Whether cluster already has MusicBrainz data
-- - How long ago identification was last attempted
--
-- This is a pure decision function with no side effects.
shouldRetryIdentification :: IdentifyConfig
                          -> UTCTime  -- ^ Current time
                          -> Maybe Text  -- ^ Current MB release ID (if any)
                          -> Maybe UTCTime  -- ^ Last identification attempt
                          -> Bool
shouldRetryIdentification config now currentMbReleaseId lastIdentifiedAt =
  case currentMbReleaseId of
    Just _ -> False  -- Already has MusicBrainz data
    Nothing -> case lastIdentifiedAt of
      Just lastTried ->
        let retryThreshold = addUTCTime (negate (cfgRetryIntervalHours config * 3600)) now
        in lastTried <= retryThreshold  -- Retry if enough time has passed
      Nothing -> True  -- Never tried, should identify
