{-# LANGUAGE OverloadedStrings #-}

-- | iTunes Search API provider for album covers.
--
-- iTunes doesn't require an API key and has good album cover coverage.
-- However, it doesn't support lookups by MBID, so we need to search by artist/album name.
module Skema.Media.Providers.ITunes
  ( fetchAlbumCover
  , fetchArtistImage
  ) where

import Skema.Media.Types
import Skema.Media.Utils (upgradeToHttps, fetchAndDecode)
import Skema.HTTP.Client (HttpClient)
import Data.Aeson (Value(..), (.:), (.:?), withObject)
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)

-- | Fetch album cover from iTunes Search API.
--
-- Searches by artist and album name, returns the highest resolution artwork.
fetchAlbumCover :: HttpClient -> Text -> Text -> IO (Either MediaError MediaResult)
fetchAlbumCover httpClient artistName albumTitle = do
  let query = encodeUtf8 $ artistName <> " " <> albumTitle
      encoded = decodeUtf8 $ urlEncode True query
      url = "https://itunes.apple.com/search?term=" <> encoded
          <> "&media=music&entity=album&limit=5"
      extractor json = do
        imageUrl <- extractAlbumCoverUrl json albumTitle
        -- iTunes returns 100x100 by default, we can get higher res
        -- Use the 100x100 as thumbnail and 600x600 as full size
        let highResUrl = upgradeToHighRes imageUrl
            thumbnailUrl = Just imageUrl  -- Original 100x100
        pure $ MediaResult
          { mrUrl = upgradeToHttps highResUrl
          , mrThumbnailUrl = fmap upgradeToHttps thumbnailUrl
          , mrSource = ITunes
          , mrQuality = Just "high"
          , mrWidth = Just 600
          , mrHeight = Just 600
          }
  fetchAndDecode httpClient ITunes url extractor

-- | Fetch artist image from iTunes.
--
-- iTunes doesn't provide artist images, only album covers.
fetchArtistImage :: HttpClient -> Text -> IO (Either MediaError MediaResult)
fetchArtistImage _ _ = pure $ Left NoMediaFound

-- | Extract album cover URL from iTunes JSON response.
--
-- Tries to match the album title to ensure we get the right album.
extractAlbumCoverUrl :: Value -> Text -> Maybe Text
extractAlbumCoverUrl (Object obj) albumTitle = do
  results <- KM.lookup "results" obj
  case results of
    Array arr -> do
      -- Find the best matching album
      let albums = mapMaybe parseAlbum (toList arr)
      findBestMatch albumTitle albums
    _ -> Nothing
extractAlbumCoverUrl _ _ = Nothing

-- | Parse an album result from iTunes.
parseAlbum :: Value -> Maybe (Text, Maybe Text)
parseAlbum v = parseMaybe parser v
  where
    parser :: Value -> Parser (Text, Maybe Text)
    parser = withObject "Album" $ \obj -> do
      title <- obj .: "collectionName"
      artworkUrl <- obj .:? "artworkUrl100"
      pure (title, artworkUrl)

-- | Find the best matching album by title similarity.
findBestMatch :: Text -> [(Text, Maybe Text)] -> Maybe Text
findBestMatch targetTitle albums =
  case sortBy (comparing (negate . similarity targetTitle . fst)) albums of
    [] -> Nothing
    (_, maybeUrl):_ -> maybeUrl
  where
    -- Simple similarity: just check if titles match (case-insensitive)
    similarity :: Text -> Text -> Int
    similarity t1 t2
      | T.toLower t1 == T.toLower t2 = 100
      | T.toLower t1 `T.isInfixOf` T.toLower t2 = 50
      | T.toLower t2 `T.isInfixOf` T.toLower t1 = 50
      | otherwise = 0

-- | Upgrade iTunes artwork URL to higher resolution.
--
-- iTunes URLs end with size specifications like "100x100bb.jpg"
-- We can replace this with "600x600bb.jpg" for better quality.
upgradeToHighRes :: Text -> Text
upgradeToHighRes url =
  T.replace "100x100bb" "600x600bb" url
