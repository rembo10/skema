{-# LANGUAGE OverloadedStrings #-}

-- | Last.fm provider for artist images and album covers.
--
-- Last.fm provides decent quality images as a fallback option.
-- Requires an API key (app-level key is hardcoded, user can override).
module Skema.Media.Providers.LastFM
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
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.List as List

-- | App-level Last.fm API key.
-- This is the default key provided for this application.
defaultApiKey :: Text
defaultApiKey = "194b59cc05ff8928dc59b42466528a2d"

-- | Fetch artist image from Last.fm.
--
-- Uses artist.getInfo API method with MBID lookup.
fetchArtistImage :: HttpClient -> Text -> MBID -> IO (Either MediaError MediaResult)
fetchArtistImage httpClient apiKey artistMBID = do
  let mbid = unMBID artistMBID
      url = "https://ws.audioscrobbler.com/2.0/?method=artist.getinfo&mbid="
          <> mbid <> "&api_key=" <> apiKey <> "&format=json"
      extractor json = do
        (imageUrl, thumbnailUrl) <- extractArtistImageUrl json
        pure $ MediaResult
          { mrUrl = upgradeToHttps imageUrl
          , mrThumbnailUrl = fmap upgradeToHttps thumbnailUrl
          , mrSource = LastFM
          , mrQuality = Just "medium"
          , mrWidth = Nothing
          , mrHeight = Nothing
          }
  fetchAndDecode httpClient LastFM url extractor

-- | Fetch album cover from Last.fm.
--
-- Uses album.getInfo API method with MBID lookup.
fetchAlbumCover :: HttpClient -> Text -> MBID -> IO (Either MediaError MediaResult)
fetchAlbumCover httpClient apiKey releaseMBID = do
  let mbid = unMBID releaseMBID
      url = "https://ws.audioscrobbler.com/2.0/?method=album.getinfo&mbid="
          <> mbid <> "&api_key=" <> apiKey <> "&format=json"
      extractor json = do
        (imageUrl, thumbnailUrl) <- extractAlbumCoverUrl json
        pure $ MediaResult
          { mrUrl = upgradeToHttps imageUrl
          , mrThumbnailUrl = fmap upgradeToHttps thumbnailUrl
          , mrSource = LastFM
          , mrQuality = Just "medium"
          , mrWidth = Nothing
          , mrHeight = Nothing
          }
  fetchAndDecode httpClient LastFM url extractor

-- | Known Last.fm placeholder image hashes to filter out.
-- These are returned when Last.fm doesn't have an actual artist image.
placeholderHashes :: [Text]
placeholderHashes =
  [ "2a96cbd8b46e442fc41c2b86b821562f"  -- Common placeholder image
  ]

-- | Check if a URL is a Last.fm placeholder image.
isPlaceholderImage :: Text -> Bool
isPlaceholderImage url = any (`T.isInfixOf` url) placeholderHashes

-- | Extract artist image URL from Last.fm JSON response.
--
-- Last.fm returns images in multiple sizes (small, medium, large, extralarge, mega).
-- We extract both a full-size image and a thumbnail.
-- Returns Nothing if only placeholder images are found.
extractArtistImageUrl :: Value -> Maybe (Text, Maybe Text)
extractArtistImageUrl (Object obj) = do
  artist <- KM.lookup "artist" obj
  case artist of
    Object artistObj -> do
      result <- extractImageUrls artistObj
      -- Filter out placeholder images
      case result of
        (url, _) | isPlaceholderImage url -> Nothing
        _ -> Just result
    _ -> Nothing
extractArtistImageUrl _ = Nothing

-- | Extract album cover URL from Last.fm JSON response.
-- Returns Nothing if only placeholder images are found.
extractAlbumCoverUrl :: Value -> Maybe (Text, Maybe Text)
extractAlbumCoverUrl (Object obj) = do
  album <- KM.lookup "album" obj
  case album of
    Object albumObj -> do
      result <- extractImageUrls albumObj
      -- Filter out placeholder images
      case result of
        (url, _) | isPlaceholderImage url -> Nothing
        _ -> Just result
    _ -> Nothing
extractAlbumCoverUrl _ = Nothing

-- | Extract both full-size and thumbnail URLs from a Last.fm image array.
-- Returns (fullSizeUrl, maybeThumbnailUrl)
extractImageUrls :: Object -> Maybe (Text, Maybe Text)
extractImageUrls obj = do
  images <- KM.lookup "image" obj
  case images of
    Array arr -> do
      let imageUrls = mapMaybe extractImageSize (toList arr)
          imageMap = fromList imageUrls

      -- Extract full-size image (prefer extralarge > large > medium)
      fullSize <- lookupImageBySize ["extralarge", "large", "medium", "mega"] imageMap

      -- Extract thumbnail (prefer medium > large > small)
      let thumbnail = lookupImageBySize ["medium", "large", "small"] imageMap

      pure (fullSize, thumbnail)
    _ -> Nothing
  where
    lookupImageBySize :: [Text] -> [(Text, Text)] -> Maybe Text
    lookupImageBySize [] _ = Nothing
    lookupImageBySize (size:rest) imageMap =
      case List.lookup size imageMap of
        Just url | not (T.null url) -> Just url
        _ -> lookupImageBySize rest imageMap

-- | Extract size and URL from an image object.
extractImageSize :: Value -> Maybe (Text, Text)
extractImageSize v = parseMaybe parser v
  where
    parser :: Value -> Parser (Text, Text)
    parser = withObject "Image" $ \obj -> do
      size <- obj .: "size"
      url <- obj .: "#text"
      pure (size, url)
