{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Library API types.
module Skema.API.Types.Library
  ( LibraryAPI
  , StatsAPI
  , ScanResponse(..)
  , FileInfo(..)
  , LibraryStats(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Library management endpoints.
type LibraryAPI = "library" :> Header "Authorization" Text :>
  ( "scan" :> Post '[JSON] ScanResponse
  :<|> "files" :> Get '[JSON] [FileInfo]
  )

-- | Stats API endpoints.
type StatsAPI = "stats" :> Header "Authorization" Text :> Get '[JSON] LibraryStats

-- | Response for library scan.
data ScanResponse = ScanResponse
  { scanSuccess :: Bool
  , scanMessage :: Text
  , scanFilesAdded :: Int
  , scanFilesModified :: Int
  , scanFilesDeleted :: Int
  , scanIdentifyRun :: Bool
    -- ^ Whether identification was run
  , scanIdentifyTotalGroups :: Maybe Int
    -- ^ Total number of file groups (albums) processed
  , scanIdentifyMatchedGroups :: Maybe Int
    -- ^ Number of groups successfully matched to MusicBrainz
  , scanIdentifyFilesUpdated :: Maybe Int
    -- ^ Number of individual files updated with MusicBrainz IDs
  } deriving (Show, Eq, Generic)

instance ToJSON ScanResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance FromJSON ScanResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

-- | File information for API responses.
data FileInfo = FileInfo
  { fileinfoPath :: Text
  , fileinfoSize :: Integer
  , fileinfoTitle :: Maybe Text
  , fileinfoArtist :: Maybe Text
  , fileinfoAlbum :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON FileInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance FromJSON FileInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Library statistics.
data LibraryStats = LibraryStats
  { statsTotalFiles :: Int
  , statsTotalAlbums :: Int
  , statsTotalArtists :: Int
  , statsMatchedFiles :: Int
  , statsUnmatchedFiles :: Int
  , statsMetadataAccuracy :: Double  -- 0-100 percentage
  , statsTotalDiffs :: Int
  , statsLibrarySize :: Integer  -- Total size in bytes
  , statsTotalRuntime :: Double  -- Total duration in seconds
  , statsLibraryPath :: Maybe Text  -- Library path
  } deriving (Show, Eq, Generic)

instance ToJSON LibraryStats where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromJSON LibraryStats where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }
