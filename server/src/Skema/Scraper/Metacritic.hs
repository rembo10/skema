{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Metacritic album review scraper.
--
-- This module scrapes album reviews from Metacritic by genre:
-- https://www.metacritic.com/browse/albums/genre/date/{genre}
--
-- Extracts:
-- - Album title
-- - Artist name
-- - Release date
-- - Critic score (0-100)
-- - User score (0-10)
-- - Genres
module Skema.Scraper.Metacritic
  ( MetacriticAlbum(..)
  , scrapeGenre
  , scrapeGenreUrl
  , parseMetacriticPage
  ) where

import Data.Time (Day)
import qualified Data.ByteString.Lazy as LBS
import Text.HTML.Scalpel ()
import Skema.Scraper.Http (fetchPage)
import Skema.Domain.Acquisition (MetacriticGenre, metacriticGenreToUrl)

-- | A scraped album from Metacritic.
data MetacriticAlbum = MetacriticAlbum
  { mcAlbumTitle :: Text
  , mcArtistName :: Text
  , mcReleaseDate :: Maybe Day
  , mcCriticScore :: Maybe Int        -- 0-100
  , mcUserScore :: Maybe Double       -- 0-10
  , mcGenres :: [MetacriticGenre]     -- Genres from the page we scraped
  , mcMetacriticUrl :: Text
  } deriving (Show, Eq)

-- | Scrape albums from a Metacritic genre page.
-- Returns a list of albums with their scores and metadata.
scrapeGenre :: MetacriticGenre -> IO (Either String [MetacriticAlbum])
scrapeGenre genre = do
  let url = buildGenreUrl genre
  scrapeGenreUrl url [genre]

-- | Build the URL for a genre's new releases page.
buildGenreUrl :: MetacriticGenre -> Text
buildGenreUrl genre =
  "https://www.metacritic.com/browse/albums/genre/date/" <> metacriticGenreToUrl genre

-- | Scrape albums from a specific URL, tagging them with genres.
scrapeGenreUrl :: Text -> [MetacriticGenre] -> IO (Either String [MetacriticAlbum])
scrapeGenreUrl url genres = do
  result <- fetchPage url
  case result of
    Left err -> pure $ Left err
    Right htmlBody -> case parseMetacriticPage htmlBody genres of
      Nothing -> pure $ Left "Failed to parse Metacritic page"
      Just albums -> pure $ Right albums

-- | Parse a Metacritic genre page HTML and extract album information.
-- NOTE: This is a placeholder implementation. The actual HTML structure needs to be verified
-- by inspecting Metacritic pages, as their HTML structure may have changed.
parseMetacriticPage :: LBS.ByteString -> [MetacriticGenre] -> Maybe [MetacriticAlbum]
parseMetacriticPage _html _genres = do
  -- For now, return an empty list as a placeholder
  -- This will be implemented once we verify the actual HTML structure
  Just []
