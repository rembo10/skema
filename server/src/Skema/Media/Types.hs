{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types for media fetching (artist images, album covers).
module Skema.Media.Types
  ( -- * Media types
    MediaType(..)
  , MediaResult(..)
  , MediaSource(..)
  , MediaError(..)
  , MediaConfig(..)
  ) where

import GHC.Generics ()
import Data.Aeson (FromJSON, ToJSON)

-- | Type of media being fetched.
data MediaType
  = ArtistImage
  | AlbumCover
  deriving (Show, Eq, Generic)

instance ToJSON MediaType
instance FromJSON MediaType

-- | Result from fetching media.
data MediaResult = MediaResult
  { mrUrl :: Text
    -- ^ URL to the full-size media
  , mrThumbnailUrl :: Maybe Text
    -- ^ URL to a thumbnail version (if available from provider)
  , mrSource :: MediaSource
    -- ^ Which provider supplied this media
  , mrQuality :: Maybe Text
    -- ^ Quality indicator (e.g., "high", "medium", "thumb")
  , mrWidth :: Maybe Int
    -- ^ Image width in pixels (if known)
  , mrHeight :: Maybe Int
    -- ^ Image height in pixels (if known)
  } deriving (Show, Eq, Generic)

instance ToJSON MediaResult
instance FromJSON MediaResult

-- | Source that provided the media.
data MediaSource
  = FanartTV
  | CoverArtArchive
  | LastFM
  | ITunes
  | MusicBrainz
  | Deezer
  deriving (Show, Eq, Ord, Generic)

instance ToJSON MediaSource
instance FromJSON MediaSource

-- | Errors that can occur during media fetching.
data MediaError
  = NoMediaFound
    -- ^ No media found from any source
  | ProviderError MediaSource Text
    -- ^ Error from a specific provider
  | NetworkError Text
    -- ^ Network/HTTP error
  | ParseError Text
    -- ^ Failed to parse provider response
  | ConfigError Text
    -- ^ Configuration error (missing API key, etc.)
  deriving (Show, Eq, Generic)

instance ToJSON MediaError
instance FromJSON MediaError

-- | Configuration for media fetching.
data MediaConfig = MediaConfig
  { mcFanartApiKey :: Maybe Text
    -- ^ fanart.tv API key (app-level key hardcoded, user can override)
  , mcLastFmApiKey :: Maybe Text
    -- ^ Last.fm API key (optional)
  , mcCacheDir :: Maybe FilePath
    -- ^ Directory for caching media files (optional)
  , mcArtistImageSources :: [MediaSource]
    -- ^ Sources to try for artist images (in priority order)
  , mcAlbumCoverSources :: [MediaSource]
    -- ^ Sources to try for album covers (in priority order)
  , mcTimeout :: Int
    -- ^ Timeout per provider request in seconds
  } deriving (Show, Eq, Generic)

instance ToJSON MediaConfig
instance FromJSON MediaConfig
