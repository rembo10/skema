{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pitchfork album review scraper.
--
-- This module scrapes album reviews from Pitchfork:
-- https://pitchfork.com/reviews/albums/
--
-- Genre-filtered URLs redirect and return 404, so genre filtering is done
-- client-side from the full listing page.
--
-- Scores are only available on individual review pages (in embedded
-- __PRELOADED_STATE__ JSON), not on the listing page. When scores are needed,
-- individual review pages are fetched with a 1-second delay between requests.
--
-- Extracts:
-- - Album title
-- - Artist name
-- - Publish date
-- - Pitchfork score (0-10, from individual review pages)
-- - Genres (from listing page rubric)
-- - Review URL
module Skema.Scraper.Pitchfork
  ( PitchforkAlbum(..)
  , scrapeReviews
  , scrapeUrl
  , parsePitchforkPage
  , fetchReviewScore
  , extractScore
  ) where

import Data.Char (isDigit)
import Data.Time (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Text.HTML.Scalpel (scrapeStringLike, chroots, text, attr, (@:), hasClass)
import Skema.Scraper.Http (fetchPage)
import Skema.Domain.Acquisition (PitchforkGenre, textToPitchforkGenre)

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
scrapeReviews :: IO (Either String [PitchforkAlbum])
scrapeReviews = scrapeUrl "https://pitchfork.com/reviews/albums/" []

-- | Scrape albums from a URL, optionally filtering by genre.
--
-- Scores are not fetched here — they must be fetched separately via
-- 'fetchReviewScore' for individual review pages.
scrapeUrl :: Text -> [PitchforkGenre] -> IO (Either String [PitchforkAlbum])
scrapeUrl url genres = do
  result <- fetchPage url
  case result of
    Left err -> pure $ Left err
    Right htmlBody -> case parsePitchforkPage htmlBody genres of
      Nothing -> pure $ Left "Failed to parse Pitchfork page"
      Just albums -> pure $ Right albums

-- | Parse a Pitchfork reviews listing page and extract album entries.
-- Filters by genre if a non-empty genre list is provided.
parsePitchforkPage :: LBS.ByteString -> [PitchforkGenre] -> Maybe [PitchforkAlbum]
parsePitchforkPage html filterGenres = do
  let htmlText = decodeUtf8 (LBS.toStrict html)
  entries <- scrapeStringLike htmlText $
    chroots ("div" @: [hasClass "summary-item"]) $ do
      -- Album title from the hed heading
      title <- text ("h3" @: [hasClass "summary-item__hed"])
      -- Artist from sub-hed
      artist <- text ("div" @: [hasClass "summary-item__sub-hed"])
      -- Genre from rubric
      genreText <- optional $ text ("span" @: [hasClass "rubric__name"])
      let genre = genreText >>= textToPitchforkGenre . T.strip
      -- Publish date
      dateText <- optional $ text ("span" @: [hasClass "summary-item__publish-date"])
      let date = dateText >>= parsePitchforkDate . T.strip
      -- Review URL from hed link
      reviewUrl <- attr "href" ("a" @: [hasClass "summary-item__hed-link"])
      pure PitchforkAlbum
        { pfAlbumTitle = T.strip title
        , pfArtistName = T.strip artist
        , pfReleaseDate = date
        , pfScore = Nothing  -- Scores fetched separately from individual pages
        , pfGenres = maybeToList genre
        , pfReviewUrl = "https://pitchfork.com" <> reviewUrl
        }
  -- Filter by genre if specified
  let filtered = case filterGenres of
        [] -> entries
        gs -> filter (\a -> any (`elem` gs) (pfGenres a)) entries
  Just filtered

-- | Fetch the review score from an individual Pitchfork review page.
-- Extracts the rating from the embedded __PRELOADED_STATE__ JSON.
fetchReviewScore :: Text -> IO (Maybe Double)
fetchReviewScore reviewUrl = do
  result <- fetchPage reviewUrl
  case result of
    Left _ -> pure Nothing
    Right body -> pure $ extractScore (decodeUtf8 $ LBS.toStrict body)

-- | Extract rating from __PRELOADED_STATE__ JSON embedded in page HTML.
-- Looks for the pattern @"rating":X.X@ in the page source.
extractScore :: Text -> Maybe Double
extractScore html =
  case T.breakOn "\"rating\":" html of
    (_, rest) | T.null rest -> Nothing
    (_, rest) ->
      let afterKey = T.drop 9 rest  -- drop "rating":
          numText = T.takeWhile (\c -> c == '.' || isDigit c) afterKey
      in readMaybe (T.unpack numText)

-- | Parse a Pitchfork date string like "March 22, 2026".
parsePitchforkDate :: Text -> Maybe Day
parsePitchforkDate t =
  parseTimeM True defaultTimeLocale "%B %e, %Y" (T.unpack t)
