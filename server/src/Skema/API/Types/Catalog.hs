{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Catalog API types.
module Skema.API.Types.Catalog
  ( CatalogAPI
  , CatalogQueryRequest(..)
  , CatalogQueryResponse(..)
  , CatalogArtistResponse(..)
  , CatalogAlbumResponse(..)
  , CreateCatalogArtistRequest(..)
  , UpdateCatalogArtistRequest(..)
  , CreateCatalogAlbumRequest(..)
  , UpdateCatalogAlbumRequest(..)
  , SearchHistoryResponse(..)
  , SearchHistoryResultResponse(..)
  ) where

import Skema.API.Types.Events (EventResponse)
import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import Data.Time (UTCTime)
import GHC.Generics ()
import Servant

-- | Catalog API endpoints.
type CatalogAPI = "catalog" :> Header "Authorization" Text :>
  ( "query" :> ReqBody '[JSON] CatalogQueryRequest :> Post '[JSON] CatalogQueryResponse
  :<|> "artists" :> QueryParam "followed" Bool :> Get '[JSON] [CatalogArtistResponse]
  :<|> "artists" :> ReqBody '[JSON] CreateCatalogArtistRequest :> PostCreated '[JSON] CatalogArtistResponse
  :<|> "artists" :> Capture "artistId" Int64 :> ReqBody '[JSON] UpdateCatalogArtistRequest :> Patch '[JSON] CatalogArtistResponse
  :<|> "artists" :> Capture "artistId" Int64 :> DeleteNoContent
  :<|> "artists" :> Capture "artistId" Int64 :> "refresh" :> Post '[JSON] EventResponse
  :<|> "refresh" :> Post '[JSON] EventResponse
  :<|> "albums" :> QueryParam "wanted" Bool :> QueryParam "artistId" Int64 :> Get '[JSON] [CatalogAlbumResponse]
  :<|> "albums" :> ReqBody '[JSON] CreateCatalogAlbumRequest :> PostCreated '[JSON] CatalogAlbumResponse
  :<|> "albums" :> Capture "albumId" Int64 :> ReqBody '[JSON] UpdateCatalogAlbumRequest :> Patch '[JSON] CatalogAlbumResponse
  :<|> "albums" :> Capture "albumId" Int64 :> DeleteNoContent
  :<|> "albums" :> Capture "albumId" Int64 :> "search-history" :> Get '[JSON] [SearchHistoryResponse]
  :<|> "albums" :> Capture "albumId" Int64 :> "search-history" :> Capture "historyId" Int64 :> "results" :> Get '[JSON] [SearchHistoryResultResponse]
  )

-- | Request for universal catalog search (searches both artists and albums).
data CatalogQueryRequest = CatalogQueryRequest
  { catalogQueryQuery :: Text
    -- ^ Search query string
  , catalogQueryLimit :: Maybe Int
    -- ^ Optional limit per type (default 10)
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogQueryRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON CatalogQueryRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

-- | Response from universal catalog search.
data CatalogQueryResponse = CatalogQueryResponse
  { catalogQueryResponseArtists :: [CatalogArtistResponse]
    -- ^ Artist search results
  , catalogQueryResponseAlbums :: [CatalogAlbumResponse]
    -- ^ Album search results
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogQueryResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

instance FromJSON CatalogQueryResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

-- | Catalog artist response.
data CatalogArtistResponse = CatalogArtistResponse
  { catalogArtistResponseId :: Maybe Int64
    -- ^ Database ID (Nothing if not yet in catalog)
  , catalogArtistResponseMBID :: Text
  , catalogArtistResponseName :: Text
  , catalogArtistResponseType :: Maybe Text
    -- ^ Artist type (e.g., "Person", "Group")
  , catalogArtistResponseImageUrl :: Maybe Text
  , catalogArtistResponseThumbnailUrl :: Maybe Text
  , catalogArtistResponseFollowed :: Bool
    -- ^ Whether user follows this artist
  , catalogArtistResponseQualityProfileId :: Maybe Int64
    -- ^ Quality profile ID for this artist (Nothing = use default)
  , catalogArtistResponseScore :: Maybe Int
    -- ^ MusicBrainz search score (0-100), present in search results
  , catalogArtistResponseCreatedAt :: Maybe Text
  , catalogArtistResponseUpdatedAt :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogArtistResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance FromJSON CatalogArtistResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

-- | Catalog album response.
data CatalogAlbumResponse = CatalogAlbumResponse
  { catalogAlbumResponseId :: Maybe Int64
    -- ^ Database ID (Nothing if not yet in catalog)
  , catalogAlbumResponseReleaseGroupMBID :: Text
  , catalogAlbumResponseTitle :: Text
  , catalogAlbumResponseArtistMBID :: Text
  , catalogAlbumResponseArtistName :: Text
  , catalogAlbumResponseType :: Maybe Text
    -- ^ Album type (e.g., "Album", "EP", "Single")
  , catalogAlbumResponseFirstReleaseDate :: Maybe Text
  , catalogAlbumResponseCoverUrl :: Maybe Text
    -- ^ Album cover artwork URL
  , catalogAlbumResponseCoverThumbnailUrl :: Maybe Text
    -- ^ Album cover thumbnail URL
  , catalogAlbumResponseWanted :: Bool
    -- ^ Whether user wants this album
  , catalogAlbumResponseMatchedClusterId :: Maybe Int64
    -- ^ ID of matched cluster in library (if any)
  , catalogAlbumResponseQualityProfileId :: Maybe Int64
    -- ^ Quality profile ID for this album (Nothing = use artist/default)
  , catalogAlbumResponseScore :: Maybe Int
    -- ^ MusicBrainz search score (0-100), present in search results
  , catalogAlbumResponseCreatedAt :: Maybe Text
  , catalogAlbumResponseUpdatedAt :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogAlbumResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

instance FromJSON CatalogAlbumResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

-- | Request to create/upsert a catalog artist.
data CreateCatalogArtistRequest = CreateCatalogArtistRequest
  { createCatalogArtistMBID :: Text
  , createCatalogArtistName :: Text
  , createCatalogArtistType :: Maybe Text
  , createCatalogArtistImageUrl :: Maybe Text
  , createCatalogArtistFollowed :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON CreateCatalogArtistRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance FromJSON CreateCatalogArtistRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

-- | Request to update a catalog artist (typically just the followed status).
data UpdateCatalogArtistRequest = UpdateCatalogArtistRequest
  { updateCatalogArtistFollowed :: Bool
  , updateCatalogArtistQualityProfileId :: Maybe (Maybe Int64)
    -- ^ Optional quality profile ID (Nothing = no change, Just Nothing = clear profile, Just (Just id) = set profile)
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateCatalogArtistRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance FromJSON UpdateCatalogArtistRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

-- | Request to create/upsert a catalog album.
data CreateCatalogAlbumRequest = CreateCatalogAlbumRequest
  { createCatalogAlbumReleaseGroupMBID :: Text
  , createCatalogAlbumTitle :: Text
  , createCatalogAlbumArtistMBID :: Text
  , createCatalogAlbumArtistName :: Text
  , createCatalogAlbumType :: Maybe Text
  , createCatalogAlbumFirstReleaseDate :: Maybe Text
  , createCatalogAlbumWanted :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON CreateCatalogAlbumRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

instance FromJSON CreateCatalogAlbumRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

-- | Request to update a catalog album (typically just the wanted status).
data UpdateCatalogAlbumRequest = UpdateCatalogAlbumRequest
  { updateCatalogAlbumWanted :: Bool
  , updateCatalogAlbumQualityProfileId :: Maybe (Maybe Int64)
    -- ^ Optional quality profile ID (Nothing = no change, Just Nothing = clear profile, Just (Just id) = set profile)
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateCatalogAlbumRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

instance FromJSON UpdateCatalogAlbumRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

-- | Search history response - one entry per search attempt.
data SearchHistoryResponse = SearchHistoryResponse
  { searchHistoryResponseId :: Int64
  , searchHistoryResponseSearchedAt :: UTCTime
  , searchHistoryResponseTotalResults :: Int
  , searchHistoryResponseDurationMs :: Maybe Int
  , searchHistoryResponseSelectedTitle :: Maybe Text
  , searchHistoryResponseSelectedIndexer :: Maybe Text
  , searchHistoryResponseSelectedScore :: Maybe Int
  , searchHistoryResponseOutcome :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SearchHistoryResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance FromJSON SearchHistoryResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

-- | Individual search result response.
data SearchHistoryResultResponse = SearchHistoryResultResponse
  { searchHistoryResultResponseId :: Int64
  , searchHistoryResultResponseIndexerName :: Text
  , searchHistoryResultResponseTitle :: Text
  , searchHistoryResultResponseDownloadUrl :: Text
  , searchHistoryResultResponseInfoUrl :: Maybe Text
  , searchHistoryResultResponseSizeBytes :: Maybe Integer
  , searchHistoryResultResponsePublishDate :: Maybe UTCTime
  , searchHistoryResultResponseSeeders :: Maybe Int
  , searchHistoryResultResponsePeers :: Maybe Int
  , searchHistoryResultResponseGrabs :: Maybe Int
  , searchHistoryResultResponseDownloadType :: Text
  , searchHistoryResultResponseQuality :: Maybe Text
  , searchHistoryResultResponseScore :: Int
  , searchHistoryResultResponseRank :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SearchHistoryResultResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 27 }

instance FromJSON SearchHistoryResultResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 27 }
