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
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Text.HTML.Scalpel (scrapeStringLike, chroots, text, attr, (@:), (//), hasClass)
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
-- Extracts albums from browse-score-clamp divs containing title, artist,
-- critic score, user score, and release date.
parseMetacriticPage :: LBS.ByteString -> [MetacriticGenre] -> Maybe [MetacriticAlbum]
parseMetacriticPage html genres =
  scrapeStringLike (decodeUtf8 $ LBS.toStrict html) $
    chroots ("div" @: [hasClass "browse-score-clamp"]) $ do
      -- Title and URL from the title link
      title <- text ("a" @: [hasClass "title"] // "h3")
      url <- attr "href" ("a" @: [hasClass "title"])
      -- Artist from clamp-details (prefixed with "by ")
      artistRaw <- text ("div" @: [hasClass "artist"])
      let artist = T.strip $ fromMaybe (T.strip artistRaw) (T.stripPrefix "by " (T.strip artistRaw))
      -- Critic score from clamp-metascore
      criticScoreText <- optional $ text ("div" @: [hasClass "clamp-metascore"] // "div" @: [hasClass "metascore_w"])
      let criticScore = criticScoreText >>= readMaybe . T.unpack . T.strip
      -- User score from clamp-userscore (may be "tbd")
      userScoreText <- optional $ text ("div" @: [hasClass "clamp-userscore"] // "div" @: [hasClass "metascore_w"])
      let userScore = userScoreText >>= parseUserScore
      -- Release date from clamp-details span
      dateText <- optional $ text ("div" @: [hasClass "clamp-details"] // "span")
      let releaseDate = dateText >>= parseMetacriticDate . T.strip
      pure MetacriticAlbum
        { mcAlbumTitle = T.strip title
        , mcArtistName = artist
        , mcReleaseDate = releaseDate
        , mcCriticScore = criticScore
        , mcUserScore = userScore
        , mcGenres = genres
        , mcMetacriticUrl = "https://www.metacritic.com" <> url
        }

-- | Parse a Metacritic date string like "February 27, 2026".
parseMetacriticDate :: Text -> Maybe Day
parseMetacriticDate t =
  parseTimeM True defaultTimeLocale "%B %e, %Y" (T.unpack t)

-- | Parse a user score, handling "tbd" as Nothing.
parseUserScore :: Text -> Maybe Double
parseUserScore t =
  let s = T.strip t
  in if s == "tbd" then Nothing else readMaybe (T.unpack s)
