{-# LANGUAGE DeriveGeneric #-}

-- | Types for catalog album queries.
--
-- This module provides query builder types for flexible album queries.
module Skema.Database.Repository.Catalog.Types
  ( AlbumQuery(..)
  , defaultAlbumQuery
  ) where

-- | Query parameters for fetching catalog albums.
data AlbumQuery = AlbumQuery
  { aqLimit :: Int
  , aqOffset :: Int
  , aqStates :: Maybe [Text]
  , aqQualities :: Maybe [Text]
  , aqArtistId :: Maybe Int64
  , aqSearch :: Maybe Text
  , aqSort :: Maybe Text
  , aqOrder :: Maybe Text
  , aqReleaseDateAfter :: Maybe Text
  , aqReleaseDateBefore :: Maybe Text
  } deriving (Show, Generic)

-- | Default album query with sensible defaults.
defaultAlbumQuery :: AlbumQuery
defaultAlbumQuery = AlbumQuery
  { aqLimit = 50
  , aqOffset = 0
  , aqStates = Nothing
  , aqQualities = Nothing
  , aqArtistId = Nothing
  , aqSearch = Nothing
  , aqSort = Nothing
  , aqOrder = Nothing
  , aqReleaseDateAfter = Nothing
  , aqReleaseDateBefore = Nothing
  }
