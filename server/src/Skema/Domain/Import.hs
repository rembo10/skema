{-# LANGUAGE OverloadedStrings #-}

-- | Pure import logic for extracting metadata from MusicBrainz releases.
module Skema.Domain.Import
  ( ReleaseMetadata(..)
  , extractReleaseMetadata
  , extractYear
  , validateDownloadPath
  , MatchDecision(..)
  , confidenceDecision
  , noMatchMessage
  , lowConfidenceMessage
  ) where

import Skema.MusicBrainz.Types (MBRelease(..))
import qualified Data.Text as T

-- | Extract release metadata from MusicBrainz release for path formatting.
data ReleaseMetadata = ReleaseMetadata
  { rmLabel :: Maybe Text
  , rmCatalogNumber :: Maybe Text
  , rmCountry :: Maybe Text
  , rmDiscCount :: Int
  } deriving (Show)

extractReleaseMetadata :: MBRelease -> ReleaseMetadata
extractReleaseMetadata release = ReleaseMetadata
  { rmLabel = mbReleaseLabel release
  , rmCatalogNumber = mbReleaseCatalogNumber release
  , rmCountry = mbReleaseCountry release
  , rmDiscCount = 1  -- TODO: Calculate from tracks
  }

-- | Extract year from date string (YYYY-MM-DD or YYYY).
extractYear :: Maybe Text -> Text
extractYear Nothing = "Unknown"
extractYear (Just dateStr) =
  case T.take 4 dateStr of
    year | T.length year == 4 -> year
    _ -> "Unknown"

-- | Validate a completed download's path before importing. Returns the
-- path on success, or the failure reason recorded against the download.
validateDownloadPath :: Maybe Text -> Either Text Text
validateDownloadPath Nothing = Left "No download path"
validateDownloadPath (Just path)
  | T.null (T.strip path) = Left "Download path is empty"
  | otherwise = Right path

-- | Whether an identified release should be imported, given its match
-- confidence against the configured threshold.
data MatchDecision
  = AcceptMatch                  -- ^ confidence clears the threshold
  | RejectLowConfidence Text     -- ^ below threshold; carries the failure message
  deriving (Show, Eq)

-- | Decide whether a match's confidence (0..1) clears the minimum.
confidenceDecision :: Double -> Double -> MatchDecision
confidenceDecision confidence minConfidence
  | confidence < minConfidence = RejectLowConfidence (lowConfidenceMessage confidence minConfidence)
  | otherwise = AcceptMatch

-- | Message recorded when MusicBrainz returns no usable match.
noMatchMessage :: Int -> Text
noMatchMessage candidateCount =
  "No MusicBrainz match found (" <> show candidateCount <> " candidates searched). " <>
  "The release may not be in MusicBrainz database, or no candidate details could be fetched."

-- | Message recorded when the best match is below the confidence threshold.
lowConfidenceMessage :: Double -> Double -> Text
lowConfidenceMessage confidence minConfidence =
  "Best match confidence (" <> show (toPercent confidence) <>
  "%) is below threshold (" <> show (toPercent minConfidence) <> "%)."

-- | Render a 0..1 confidence as a whole-number percentage.
toPercent :: Double -> Integer
toPercent x = round (x * 100)
