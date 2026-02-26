{-# LANGUAGE OverloadedStrings #-}

-- | Pure import logic for extracting metadata from MusicBrainz releases.
module Skema.Domain.Import
  ( ReleaseMetadata(..)
  , extractReleaseMetadata
  , extractYear
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
