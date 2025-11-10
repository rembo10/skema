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

import qualified Data.Text as T
import Data.Time (Day)
import qualified Data.ByteString.Lazy as LBS
import Text.HTML.Scalpel ()
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Skema.Services.Filters (PitchforkGenre, pitchforkGenreToUrl)

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
  manager <- newManager tlsManagerSettings
  req <- parseRequest (T.unpack url)
  let reqWithHeaders = req
        { requestHeaders =
            [ ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
            , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
            , ("Accept-Language", "en-US,en;q=0.5")
            ]
        }

  response <- httpLbs reqWithHeaders manager
  let htmlBody = responseBody response

  case parsePitchforkPage htmlBody genres of
    Nothing -> return $ Left "Failed to parse Pitchfork page"
    Just albums -> return $ Right albums

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
