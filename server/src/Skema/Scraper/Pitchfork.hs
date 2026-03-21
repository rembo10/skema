{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pitchfork album review scraper.
--
-- This module scrapes album reviews from Pitchfork:
-- https://pitchfork.com/reviews/albums/
--
-- Can also scrape by genre:
-- https://pitchfork.com/reviews/albums/?genre={genre}
--
-- Extracts:
-- - Album title
-- - Artist name
-- - Release date
-- - Pitchfork score (0-10)
-- - Genres (albums can have multiple genres)
module Skema.Scraper.Pitchfork
  ( PitchforkAlbum(..)
  , scrapeReviews
  , scrapeGenre
  , scrapeUrl
  , parsePitchforkPage
  ) where

import Data.Time (Day)
import qualified Data.ByteString.Lazy as LBS
import Text.HTML.Scalpel ()
import Skema.Scraper.Http (fetchPage)
import Skema.Domain.Acquisition (PitchforkGenre, pitchforkGenreToUrl)

-- | A scraped album from Pitchfork.
data PitchforkAlbum = PitchforkAlbum
  { pfAlbumTitle :: Text
  , pfArtistName :: Text
  , pfReleaseDate :: Maybe Day
  , pfScore :: Maybe Double           -- 0-10 scale
  , pfGenres :: [PitchforkGenre]      -- Genres from the review
  , pfReviewUrl :: Text
  } deriving (Show, Eq)

-- | Scrape recent album reviews from Pitchfork's main reviews page.
-- Returns a list of albums with their scores and metadata.
scrapeReviews :: IO (Either String [PitchforkAlbum])
scrapeReviews = scrapeUrl "https://pitchfork.com/reviews/albums/" []

-- | Scrape albums from a specific Pitchfork genre page.
scrapeGenre :: PitchforkGenre -> IO (Either String [PitchforkAlbum])
scrapeGenre genre = do
  let url = "https://pitchfork.com/reviews/albums/?genre=" <> pitchforkGenreToUrl genre
  scrapeUrl url [genre]

-- | Scrape albums from a specific URL, tagging them with genres.
scrapeUrl :: Text -> [PitchforkGenre] -> IO (Either String [PitchforkAlbum])
scrapeUrl url genres = do
  result <- fetchPage url
  case result of
    Left err -> pure $ Left err
    Right htmlBody -> case parsePitchforkPage htmlBody genres of
      Nothing -> pure $ Left "Failed to parse Pitchfork page"
      Just albums -> pure $ Right albums

-- | Parse a Pitchfork reviews page HTML and extract album information.
-- NOTE: This is a placeholder implementation. The actual HTML structure needs to be verified
-- by inspecting Pitchfork pages, as their HTML structure may have changed.
parsePitchforkPage :: LBS.ByteString -> [PitchforkGenre] -> Maybe [PitchforkAlbum]
parsePitchforkPage _html _genres = do
  -- For now, return an empty list as a placeholder
  -- This will be implemented once we verify the actual HTML structure
  --
  -- Typical structure to look for:
  -- - Review cards/list items
  -- - Album title in a heading
  -- - Artist name (often separate from title)
  -- - Score (typically displayed prominently, 0-10)
  -- - Review URL
  -- - Genres may be in metadata or tags
  Just []
