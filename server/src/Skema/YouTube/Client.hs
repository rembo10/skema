{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | YouTube Data API v3 client for fetching playlist items.
-- Used to track artists from YouTube Music playlists.
module Skema.YouTube.Client
  ( YouTubePlaylistItem(..)
  , YouTubeArtistTrack(..)
  , fetchPlaylistItems
  , parseArtistFromVideo
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Network.HTTP.Simple
  ( httpJSON
  , getResponseBody
  , parseRequest
  , setRequestQueryString
  )
import qualified Data.ByteString.Char8 as BS
import Control.Exception (try, SomeException)

-- | A single item from a YouTube playlist.
data YouTubePlaylistItem = YouTubePlaylistItem
  { ypiVideoId :: Text
  , ypiTitle :: Text
  , ypiChannelTitle :: Text
  , ypiDescription :: Maybe Text
  , ypiVideoOwnerChannelTitle :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Parsed artist and track from a YouTube Music video.
data YouTubeArtistTrack = YouTubeArtistTrack
  { yatArtistName :: Text
  , yatTrackTitle :: Text
  , yatVideoId :: Text
  } deriving (Show, Eq, Generic)

-- | YouTube API response structures
data YouTubePlaylistResponse = YouTubePlaylistResponse
  { yprItems :: [YouTubePlaylistItemWrapper]
  , yprNextPageToken :: Maybe Text
  } deriving (Show, Eq, Generic)

data YouTubePlaylistItemWrapper = YouTubePlaylistItemWrapper
  { ypiwSnippet :: YouTubeSnippet
  } deriving (Show, Eq, Generic)

data YouTubeSnippet = YouTubeSnippet
  { ysTitle :: Text
  , ysChannelTitle :: Text
  , ysDescription :: Maybe Text
  , ysVideoOwnerChannelTitle :: Maybe Text
  , ysResourceId :: YouTubeResourceId
  } deriving (Show, Eq, Generic)

data YouTubeResourceId = YouTubeResourceId
  { yrVideoId :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON YouTubePlaylistResponse where
  parseJSON = withObject "YouTubePlaylistResponse" $ \o ->
    YouTubePlaylistResponse
      <$> o .: "items"
      <*> o .:? "nextPageToken"

instance FromJSON YouTubePlaylistItemWrapper where
  parseJSON = withObject "YouTubePlaylistItemWrapper" $ \o ->
    YouTubePlaylistItemWrapper
      <$> o .: "snippet"

instance FromJSON YouTubeSnippet where
  parseJSON = withObject "YouTubeSnippet" $ \o ->
    YouTubeSnippet
      <$> o .: "title"
      <*> o .: "channelTitle"
      <*> o .:? "description"
      <*> o .:? "videoOwnerChannelTitle"
      <*> o .: "resourceId"

instance FromJSON YouTubeResourceId where
  parseJSON = withObject "YouTubeResourceId" $ \o ->
    YouTubeResourceId
      <$> o .: "videoId"

-- | Fetch all items from a YouTube playlist.
-- Handles pagination automatically.
fetchPlaylistItems :: Text -> Text -> IO (Either Text [YouTubePlaylistItem])
fetchPlaylistItems apiKey playlistId = do
  fetchPage Nothing []
  where
    fetchPage :: Maybe Text -> [YouTubePlaylistItem] -> IO (Either Text [YouTubePlaylistItem])
    fetchPage pageToken acc = do
      result <- fetchSinglePage apiKey playlistId pageToken
      case result of
        Left err -> pure $ Left err
        Right (items, nextToken) ->
          let newAcc = acc ++ items
          in case nextToken of
            Nothing -> pure $ Right newAcc
            Just token -> fetchPage (Just token) newAcc

-- | Fetch a single page of playlist items.
fetchSinglePage :: Text -> Text -> Maybe Text -> IO (Either Text ([YouTubePlaylistItem], Maybe Text))
fetchSinglePage apiKey playlistId pageToken = do
  let baseUrl = "https://www.googleapis.com/youtube/v3/playlistItems"
      queryParams = 
        [ ("part", Just "snippet")
        , ("playlistId", Just $ BS.pack $ toString playlistId)
        , ("key", Just $ BS.pack $ toString apiKey)
        , ("maxResults", Just "50")
        ] ++ maybe [] (\t -> [("pageToken", Just $ BS.pack $ toString t)]) pageToken
  
  request <- parseRequest baseUrl
  let requestWithParams = setRequestQueryString queryParams request
  
  response <- try $ httpJSON requestWithParams
  case response of
    Left (err :: SomeException) -> 
      pure $ Left $ "YouTube API error: " <> show err
    Right resp -> do
      let body = getResponseBody resp :: YouTubePlaylistResponse
          items = map wrapperToItem (yprItems body)
      pure $ Right (items, yprNextPageToken body)

-- | Convert wrapper to simplified item.
wrapperToItem :: YouTubePlaylistItemWrapper -> YouTubePlaylistItem
wrapperToItem wrapper = YouTubePlaylistItem
  { ypiVideoId = yrVideoId (ysResourceId (ypiwSnippet wrapper))
  , ypiTitle = ysTitle (ypiwSnippet wrapper)
  , ypiChannelTitle = ysChannelTitle (ypiwSnippet wrapper)
  , ypiDescription = ysDescription (ypiwSnippet wrapper)
  , ypiVideoOwnerChannelTitle = ysVideoOwnerChannelTitle (ypiwSnippet wrapper)
  }

-- | Parse artist name from a YouTube Music video.
-- YouTube Music videos typically have structured metadata.
-- The channel title is usually the artist name for official uploads.
-- For user playlists, we try to parse "Artist - Track" format from title.
parseArtistFromVideo :: YouTubePlaylistItem -> Maybe YouTubeArtistTrack
parseArtistFromVideo item =
  -- Strategy 1: Use videoOwnerChannelTitle if available (most reliable for YT Music)
  -- Strategy 2: Try to parse "Artist - Track" from title
  -- Strategy 3: Use channel title as artist
  let title = ypiTitle item
      videoId = ypiVideoId item
      
      -- Try parsing "Artist - Track" format
      parsedFromTitle = case T.splitOn " - " title of
        [artist, track] -> Just (cleanArtistName $ T.strip artist, T.strip track)
        -- Handle "Artist - Track (Official Video)" etc.
        (artist : rest) | length rest >= 1 -> 
          Just (cleanArtistName $ T.strip artist, T.strip $ T.intercalate " - " rest)
        _ -> Nothing
      
      -- Get artist from channel (fallback) - clean the " - Topic" suffix
      channelArtist = cleanArtistName $ fromMaybe (ypiChannelTitle item) (ypiVideoOwnerChannelTitle item)
      
  in case parsedFromTitle of
    Just (artist, track) -> Just $ YouTubeArtistTrack artist track videoId
    Nothing -> 
      -- Use channel as artist, title as track
      -- Skip if title looks like it's not a song (e.g., "Playlist", "Mix", etc.)
      if isLikelySong title
        then Just $ YouTubeArtistTrack channelArtist title videoId
        else Nothing

-- | Clean up artist name by removing YouTube Music suffixes like " - Topic"
-- Also removes trailing " -" or "- " artifacts from splitting
cleanArtistName :: Text -> Text
cleanArtistName name =
  let -- First remove " - Topic" variants
      withoutTopic = removeIfSuffix " - Topic" 
                   $ removeIfSuffix " - topic"
                   $ removeIfSuffix "- Topic"
                   $ removeIfSuffix " Topic"
                   $ T.strip name
      -- Then strip any trailing hyphens/dashes (with or without spaces)
      cleaned = T.dropWhileEnd (\c -> c == '-' || c == ' ') withoutTopic
  in T.strip cleaned
  where
    removeIfSuffix suffix txt =
      if T.isSuffixOf suffix txt
        then T.dropEnd (T.length suffix) txt
        else txt

-- | Check if a title looks like a song title.
isLikelySong :: Text -> Bool
isLikelySong title =
  let lower = T.toLower title
      skipPatterns = ["playlist", "mix", "compilation", "best of", "top ", "album"]
  in not $ any (`T.isInfixOf` lower) skipPatterns
