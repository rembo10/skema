{-# LANGUAGE OverloadedStrings #-}

-- | Cover Art Archive provider for album artwork.
--
-- Cover Art Archive is the official MusicBrainz project for album covers.
-- It's free, requires no API key, and has excellent coverage.
module Skema.Media.Providers.CoverArtArchive
  ( fetchAlbumCover
  , fetchArtistImage
  ) where

import Skema.Media.Types
import Skema.Media.Utils (upgradeToHttps, fetchAndDecode)
import Skema.MusicBrainz.Types (MBID, unMBID)
import Skema.HTTP.Client (HttpClient)
import Data.Aeson (Value(..), (.:), (.:?), withObject)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM

-- | Fetch album cover from Cover Art Archive.
--
-- Uses the release-group MBID to fetch the front cover.
-- Cover Art Archive will redirect to a specific release with cover art.
-- Returns the highest quality image available.
fetchAlbumCover :: HttpClient -> MBID -> IO (Either MediaError MediaResult)
fetchAlbumCover httpClient releaseGroupMBID = do
  let mbid = unMBID releaseGroupMBID
      url = "https://coverartarchive.org/release-group/" <> mbid
      extractor json = do
        (coverUrl, thumbnailUrl) <- extractCoverUrl json
        pure $ MediaResult
          { mrUrl = coverUrl
          , mrThumbnailUrl = thumbnailUrl
          , mrSource = CoverArtArchive
          , mrQuality = Just "high"
          , mrWidth = Nothing
          , mrHeight = Nothing
          }
  fetchAndDecode httpClient CoverArtArchive url extractor

-- | Fetch artist image from Cover Art Archive.
--
-- Cover Art Archive doesn't directly support artist images, so this returns NoMediaFound.
-- However, we keep this function for consistency with the provider interface.
fetchArtistImage :: HttpClient -> MBID -> IO (Either MediaError MediaResult)
fetchArtistImage _ _ = pure $ Left NoMediaFound

-- | Extract the front cover URL from Cover Art Archive JSON response.
--
-- Prioritizes:
-- 1. Front cover image (front: true)
-- 2. First available image
--
-- Returns (fullSizeUrl, maybeThumbnailUrl)
extractCoverUrl :: Value -> Maybe (Text, Maybe Text)
extractCoverUrl (Object obj) = do
  images <- KM.lookup "images" obj
  case images of
    Array arr -> do
      -- Try to find front cover first
      let frontCover = find isFrontCover (toList arr)
      case frontCover of
        Just cover -> extractImageUrls cover
        Nothing -> do
          -- No front cover, use first image
          firstImage <- viaNonEmpty head (toList arr)
          extractImageUrls firstImage
    _ -> Nothing
extractCoverUrl _ = Nothing

-- | Check if an image object is marked as the front cover.
isFrontCover :: Value -> Bool
isFrontCover (Object obj) =
  case KM.lookup "front" obj of
    Just (Bool True) -> True
    _ -> False
isFrontCover _ = False

-- | Extract both full-size and thumbnail URLs from an image object.
--
-- Cover Art Archive provides both "image" (full-size) and "thumbnails" fields.
-- Returns (fullSizeUrl, maybeThumbnailUrl)
extractImageUrls :: Value -> Maybe (Text, Maybe Text)
extractImageUrls v = parseMaybe parser v
  where
    parser = withObject "Image" $ \o -> do
      fullSize <- o .: "image"
      -- Try to get thumbnail (small or large thumbnail)
      maybeThumbnails <- o .:? "thumbnails"
      thumbnail <- case maybeThumbnails of
        Just (Object thumbObj) ->
          -- Prefer "large" thumbnail (~500px), fallback to "small" (~250px)
          case KM.lookup "large" thumbObj of
            Just (String url) -> pure $ Just (upgradeToHttps url)
            _ -> case KM.lookup "small" thumbObj of
              Just (String url) -> pure $ Just (upgradeToHttps url)
              _ -> pure Nothing
        _ -> pure Nothing
      pure (upgradeToHttps fullSize, thumbnail)
