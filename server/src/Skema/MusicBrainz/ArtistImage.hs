{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Artist image fetching from multiple sources.
--
-- This module provides functions to fetch artist images from various sources
-- in order of preference, returning the first available image URL.
module Skema.MusicBrainz.ArtistImage
  ( fetchArtistImage
  , ArtistImageSource(..)
  ) where

import Skema.MusicBrainz.Types (MBID, unMBID)
import Skema.MusicBrainz.Client (MBClientEnv(..))
import Skema.HTTP.Client (HttpClient)
import qualified Skema.HTTP.Client as HTTP
import Data.Aeson (decode, Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Katip

-- | Available artist image sources.
data ArtistImageSource
  = MusicBrainzWikipedia  -- Extracts image from Wikipedia via MusicBrainz relationships
  | CommonsMusicBrainz    -- Uses Wikimedia Commons via MusicBrainz
  | FanartTV              -- fanart.tv (requires API key - not yet implemented)
  | LastFM                -- Last.fm (requires API key - not yet implemented)
  deriving (Show, Eq)

-- | Fetch an artist image URL from multiple sources.
--
-- Tries sources in order until one succeeds:
-- 1. MusicBrainz artist relations (Wikipedia/Commons)
-- 2. TODO: fanart.tv (when API key is configured)
-- 3. TODO: Last.fm (when API key is configured)
--
-- Returns Nothing if no image could be found from any source.
fetchArtistImage :: MBClientEnv -> LogEnv -> MBID -> IO (Maybe Text)
fetchArtistImage MBClientEnv{..} le artistMBID = do
  let mbid = unMBID artistMBID

  runKatipContextT le () "musicbrainz.artist_image" $ do
    katipAddContext (sl "artist_mbid" mbid) $ do
      $(logTM) DebugS $ logStr ("Fetching artist image..." :: Text)

      -- Try MusicBrainz first (free, no API key needed)
      -- Uses centralized HTTP client for proper rate limiting
      mbImage <- liftIO $ tryMusicBrainzImage mbHttpClient mbid

      case mbImage of
        Just url -> do
          $(logTM) DebugS $ logStr $ ("Found image from MusicBrainz: " <> url :: Text)
          pure $ Just url
        Nothing -> do
          $(logTM) DebugS $ logStr ("No image found from any source" :: Text)
          pure Nothing
          -- TODO: Try other sources (fanart.tv, Last.fm) when API keys are configured

-- | Try to fetch artist image from MusicBrainz relationships.
--
-- MusicBrainz doesn't host artist images directly, but artists may have
-- relationships to Wikipedia or Wikimedia Commons that contain image URLs.
-- This requires parsing the artist's relationships.
--
-- Uses the centralized HTTP client for proper rate limiting.
tryMusicBrainzImage :: HttpClient -> Text -> IO (Maybe Text)
tryMusicBrainzImage httpClient mbid = do
  -- Fetch artist data with URL relationships
  -- The centralized HTTP client will handle rate limiting, retries, and user agent
  let url = "https://musicbrainz.org/ws/2/artist/" <> mbid <> "?inc=url-rels&fmt=json"

  result <- HTTP.get httpClient url

  case result of
    Left _err -> pure Nothing  -- HTTP error, no image available
    Right body ->
      case decode body of
        Nothing -> pure Nothing
        Just json -> pure $ extractImageFromRelations json

-- | Extract image URL from MusicBrainz artist relations.
--
-- Looks for:
-- - Wikimedia Commons images
-- - Wikipedia pages (would need additional parsing)
-- - Official websites (might have images, but complex)
extractImageFromRelations :: Value -> Maybe Text
extractImageFromRelations (Object obj) = do
  relations <- KM.lookup "relations" obj
  case relations of
    Array arr -> viaNonEmpty head $ catMaybes $ map extractImageFromRelation (toList arr)
    _ -> Nothing
extractImageFromRelations _ = Nothing

-- | Extract image URL from a single MusicBrainz relation.
extractImageFromRelation :: Value -> Maybe Text
extractImageFromRelation (Object rel) = do
  -- Look for image type relations
  relType <- KM.lookup "type" rel
  case relType of
    String "image" -> do
      -- Get the URL
      urlObj <- KM.lookup "url" rel
      case urlObj of
        Object u -> do
          resource <- KM.lookup "resource" u
          case resource of
            String imgUrl -> Just $ convertToDirectImageUrl imgUrl
            _ -> Nothing
        _ -> Nothing
    String "commons image" -> do
      -- Wikimedia Commons image
      urlObj <- KM.lookup "url" rel
      case urlObj of
        Object u -> do
          resource <- KM.lookup "resource" u
          case resource of
            String imgUrl -> Just $ convertToDirectImageUrl imgUrl
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
extractImageFromRelation _ = Nothing

-- | Convert Wikimedia Commons wiki page URLs to direct image URLs.
--
-- Converts URLs like:
--   https://commons.wikimedia.org/wiki/File:Paolo_Nutini-2019.jpg
-- To:
--   https://commons.wikimedia.org/wiki/Special:FilePath/Paolo_Nutini-2019.jpg
--
-- The Special:FilePath endpoint serves the actual image file instead of the wiki page.
convertToDirectImageUrl :: Text -> Text
convertToDirectImageUrl url
  | "commons.wikimedia.org/wiki/File:" `T.isInfixOf` url =
      T.replace "/wiki/File:" "/wiki/Special:FilePath/" url
  | otherwise = url
