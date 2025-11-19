{-# LANGUAGE OverloadedStrings #-}

-- | fanart.tv provider for artist images and album covers.
--
-- fanart.tv provides high-quality artist images and album covers.
-- Requires an API key (app-level key is hardcoded, user can override).
module Skema.Media.Providers.FanartTV
  ( fetchArtistImage
  , fetchAlbumCover
  , defaultApiKey
  ) where

import Skema.Media.Types
import Skema.Media.Utils (upgradeToHttps, fetchAndDecode)
import Skema.MusicBrainz.Types (MBID, unMBID)
import Skema.HTTP.Client (HttpClient)
import Data.Aeson (Value(..), (.:), withObject)
import Data.Aeson.Types (parseMaybe, Parser, Object)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

-- | App-level fanart.tv API key.
-- This is the default key provided by the user for this application.
defaultApiKey :: Text
defaultApiKey = "c93aada7a9e3ac2bfc41e25520dcf6ed"

-- | Fetch artist image from fanart.tv.
--
-- Prioritizes high-quality images:
-- 1. Artist thumbs (high quality)
-- 2. HD music logos (if available)
-- 3. Music logos (fallback)
fetchArtistImage :: HttpClient -> Text -> MBID -> IO (Either MediaError MediaResult)
fetchArtistImage httpClient apiKey artistMBID = do
  let mbid = unMBID artistMBID
      url = "https://webservice.fanart.tv/v3/music/" <> mbid <> "?api_key=" <> apiKey
      extractor json = do
        (imageUrl, quality) <- extractArtistImageUrl json
        pure $ MediaResult
          { mrUrl = upgradeToHttps imageUrl
          , mrThumbnailUrl = Nothing
          , mrSource = FanartTV
          , mrQuality = Just quality
          , mrWidth = Nothing
          , mrHeight = Nothing
          }
  fetchAndDecode httpClient FanartTV url extractor

-- | Fetch album cover from fanart.tv.
--
-- Uses release MBID to find album art.
fetchAlbumCover :: HttpClient -> Text -> MBID -> IO (Either MediaError MediaResult)
fetchAlbumCover httpClient apiKey releaseMBID = do
  let mbid = unMBID releaseMBID
      url = "https://webservice.fanart.tv/v3/music/albums/" <> mbid <> "?api_key=" <> apiKey
      extractor json = do
        imageUrl <- extractAlbumCoverUrl json
        pure $ MediaResult
          { mrUrl = upgradeToHttps imageUrl
          , mrThumbnailUrl = Nothing
          , mrSource = FanartTV
          , mrQuality = Just "high"
          , mrWidth = Nothing
          , mrHeight = Nothing
          }
  fetchAndDecode httpClient FanartTV url extractor

-- | Extract artist image URL from fanart.tv JSON response.
--
-- Priority order:
-- 1. artistthumb (high quality artist images)
-- 2. hdmusiclogo (HD logos)
-- 3. musiclogo (standard logos)
extractArtistImageUrl :: Value -> Maybe (Text, Text)
extractArtistImageUrl (Object obj) =
  -- Try artist thumbs first (best quality)
  tryExtractArray obj "artistthumb" "high"
    <|> tryExtractArray obj "hdmusiclogo" "hd"
    <|> tryExtractArray obj "musiclogo" "medium"
extractArtistImageUrl _ = Nothing

-- | Extract album cover URL from fanart.tv JSON response.
extractAlbumCoverUrl :: Value -> Maybe Text
extractAlbumCoverUrl (Object obj) = do
  albums <- KM.lookup "albums" obj
  case albums of
    Object albumsObj ->
      -- fanart.tv returns albums keyed by release group MBID
      -- We take the first available album art
      case toList (KM.toList albumsObj) of
        [] -> Nothing
        ((_, albumData):_) -> extractAlbumArtFromData albumData
    _ -> Nothing
extractAlbumCoverUrl _ = Nothing

-- | Extract album art URL from album data object.
extractAlbumArtFromData :: Value -> Maybe Text
extractAlbumArtFromData (Object obj) =
  tryExtractArrayUrl obj "albumcover"
    <|> tryExtractArrayUrl obj "cdart"
extractAlbumArtFromData _ = Nothing

-- | Try to extract URL from an array field (just URL, no quality).
tryExtractArrayUrl :: Object -> Text -> Maybe Text
tryExtractArrayUrl obj fieldName = do
  field <- KM.lookup (K.fromText fieldName) obj
  case field of
    Array arr -> do
      firstItem <- viaNonEmpty head (toList arr)
      parseMaybe urlParser firstItem
    _ -> Nothing
  where
    urlParser :: Value -> Parser Text
    urlParser = withObject "URL" $ \obj' -> obj' .: "url"

-- | Try to extract URL and quality from an array field.
tryExtractArray :: Object -> Text -> Text -> Maybe (Text, Text)
tryExtractArray obj fieldName quality = do
  field <- KM.lookup (K.fromText fieldName) obj
  case field of
    Array arr -> do
      firstItem <- viaNonEmpty head (toList arr)
      url <- parseMaybe urlParser firstItem
      Just (url, quality)
    _ -> Nothing
  where
    urlParser :: Value -> Parser Text
    urlParser = withObject "URL" $ \obj' -> obj' .: "url"
