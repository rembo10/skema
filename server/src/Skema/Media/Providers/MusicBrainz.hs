{-# LANGUAGE OverloadedStrings #-}

-- | MusicBrainz provider for artist images.
--
-- MusicBrainz doesn't host images directly, but provides relationships to
-- Wikipedia and Wikimedia Commons that may contain image URLs.
-- This is a fallback option with limited coverage.
module Skema.Media.Providers.MusicBrainz
  ( fetchArtistImage
  , fetchAlbumCover
  ) where

import Skema.Media.Types
import Skema.Media.Utils (upgradeToHttps, fetchAndDecode)
import Skema.MusicBrainz.Types (MBID, unMBID)
import Skema.HTTP.Client (HttpClient)
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

-- | Fetch artist image from MusicBrainz relationships.
--
-- Looks for image or commons image relationships.
fetchArtistImage :: HttpClient -> MBID -> IO (Either MediaError MediaResult)
fetchArtistImage httpClient artistMBID = do
  let mbid = unMBID artistMBID
      url = "https://musicbrainz.org/ws/2/artist/" <> mbid <> "?inc=url-rels&fmt=json"
      extractor json = do
        imageUrl <- extractImageFromRelations json
        pure $ MediaResult
          { mrUrl = upgradeToHttps imageUrl
          , mrThumbnailUrl = Nothing
          , mrSource = MusicBrainz
          , mrQuality = Just "medium"
          , mrWidth = Nothing
          , mrHeight = Nothing
          }
  fetchAndDecode httpClient MusicBrainz url extractor

-- | Fetch album cover from MusicBrainz.
--
-- MusicBrainz doesn't host album covers directly, use Cover Art Archive instead.
fetchAlbumCover :: HttpClient -> MBID -> IO (Either MediaError MediaResult)
fetchAlbumCover _ _ = pure $ Left NoMediaFound

-- | Extract image URL from MusicBrainz artist relations.
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
  relType <- KM.lookup "type" rel
  case relType of
    String "image" -> extractUrlFromRelation rel
    String "commons image" -> extractUrlFromRelation rel
    _ -> Nothing
extractImageFromRelation _ = Nothing

-- | Extract URL from a relation object.
extractUrlFromRelation :: Object -> Maybe Text
extractUrlFromRelation rel = do
  urlObj <- KM.lookup "url" rel
  case urlObj of
    Object u -> do
      resource <- KM.lookup "resource" u
      case resource of
        String imgUrl -> Just $ convertToDirectImageUrl imgUrl
        _ -> Nothing
    _ -> Nothing

-- | Convert Wikimedia Commons wiki page URLs to direct image URLs.
--
-- Converts URLs like:
--   https://commons.wikimedia.org/wiki/File:Paolo_Nutini-2019.jpg
-- To:
--   https://commons.wikimedia.org/wiki/Special:FilePath/Paolo_Nutini-2019.jpg
convertToDirectImageUrl :: Text -> Text
convertToDirectImageUrl url
  | "commons.wikimedia.org/wiki/File:" `T.isInfixOf` url =
      T.replace "/wiki/File:" "/wiki/Special:FilePath/" url
  | otherwise = url
