{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Catalog API types.
module Skema.API.Types.Catalog
  ( CatalogAPI
  , CatalogQueryRequest(..)
  , CatalogQueryResponse(..)
  , CatalogArtistResponse(..)
  , ArtistsPagination(..)
  , ArtistsResponse(..)
  , CatalogAlbumResponse(..)
  , CatalogAlbumOverviewResponse(..)
  , AlbumOverviewResponse(..)
  , CatalogAlbumOverviewRequest(..)
  , AlbumOverviewPagination(..)
  , AlbumOverviewStats(..)
  , AlbumState(..)
  , ActiveDownloadInfo(..)
  , BulkAlbumActionRequest(..)
  , BulkAlbumAction(..)
  , CreateCatalogArtistRequest(..)
  , UpdateCatalogArtistRequest(..)
  , CreateCatalogAlbumRequest(..)
  , UpdateCatalogAlbumRequest(..)
  , CatalogTaskRequest(..)
  , AlbumReleasesResponse(..)
  , ReleaseResponse(..)
  , SlskdFileResponse(..)
  , ReleaseStreamEvent(..)
  ) where

import Skema.API.Types.Tasks (TaskResponse)
import Skema.API.Types.Common (SourceIO)
import Skema.Core.Catalog (AlbumState(..))
import Data.Aeson (ToJSON(..), FromJSON(..), Value, defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2, withObject, (.:), object, (.=), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import GHC.Generics ()
import Servant hiding (SourceIO, ServerSentEvents)
import Servant.API.EventStream (ServerSentEvents, ToServerEvent(..))
import qualified Servant.API.EventStream as SSE

-- | Catalog API endpoints.
type CatalogAPI = "catalog" :> Header "Authorization" Text :>
  ( "tasks" :> ReqBody '[JSON] CatalogTaskRequest :> PostCreated '[JSON] TaskResponse
  :<|> "query" :> ReqBody '[JSON] CatalogQueryRequest :> Post '[JSON] CatalogQueryResponse
  :<|> "artists"
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> QueryParam "followed" Bool
    :> QueryParam "search" Text
    :> QueryParam "sort" Text
    :> QueryParam "order" Text
    :> Get '[JSON] ArtistsResponse
  :<|> "artists" :> ReqBody '[JSON] CreateCatalogArtistRequest :> PostCreated '[JSON] CatalogArtistResponse
  :<|> "artists" :> Capture "artistId" Int64 :> ReqBody '[JSON] UpdateCatalogArtistRequest :> Patch '[JSON] CatalogArtistResponse
  :<|> "artists" :> Capture "artistId" Int64 :> DeleteNoContent
  :<|> "albums"
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> QueryParam "wanted" Bool
    :> QueryParam "artistId" Int64
    :> QueryParam "search" Text
    :> QueryParam "sort" Text
    :> QueryParam "order" Text
    :> QueryParam "state" Text
    :> QueryParam "quality" Text
    :> QueryParam "release_date_after" Text
    :> QueryParam "release_date_before" Text
    :> Get '[JSON] AlbumOverviewResponse
  :<|> "albums" :> ReqBody '[JSON] CreateCatalogAlbumRequest :> PostCreated '[JSON] CatalogAlbumResponse
  :<|> "albums" :> Capture "albumId" Int64 :> ReqBody '[JSON] UpdateCatalogAlbumRequest :> Patch '[JSON] CatalogAlbumResponse
  :<|> "albums" :> Capture "albumId" Int64 :> DeleteNoContent
  :<|> "albums" :> Capture "albumId" Int64 :> "releases" :> Get '[JSON] AlbumReleasesResponse
  :<|> "albums" :> Capture "albumId" Int64 :> "releases" :> "stream" :> QueryParam "token" Text :> ServerSentEvents (SourceIO ReleaseStreamEvent)
  :<|> "albums" :> "bulk-action" :> ReqBody '[JSON] BulkAlbumActionRequest :> Post '[JSON] NoContent
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
  , catalogArtistResponseAlbums :: Maybe [CatalogAlbumOverviewResponse]
    -- ^ Recent albums for this artist (optional, included for followed artists list)
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogArtistResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance FromJSON CatalogArtistResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

-- | Pagination info for artists.
data ArtistsPagination = ArtistsPagination
  { artistsPaginationTotal :: Int
  , artistsPaginationOffset :: Int
  , artistsPaginationLimit :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistsPagination where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }

instance FromJSON ArtistsPagination where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }

-- | Paginated artists response.
data ArtistsResponse = ArtistsResponse
  { artistsResponsePagination :: ArtistsPagination
  , artistsResponseArtists :: [CatalogArtistResponse]
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistsResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON ArtistsResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

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

-- | Request to update a catalog album's quality profile.
-- Note: wanted status is derived automatically from quality_profile_id on the backend
data UpdateCatalogAlbumRequest = UpdateCatalogAlbumRequest
  { updateCatalogAlbumQualityProfileId :: Maybe (Maybe Int64)
    -- ^ Optional quality profile ID (Nothing = no change, Just Nothing = clear profile, Just (Just id) = set profile)
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateCatalogAlbumRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

instance FromJSON UpdateCatalogAlbumRequest where
  parseJSON = withObject "UpdateCatalogAlbumRequest" $ \o -> do
    -- Explicitly handle the quality_profile_id field to distinguish between:
    -- - Field missing -> Nothing (no change)
    -- - Field present with null -> Just Nothing (clear/unwant)
    -- - Field present with value -> Just (Just value) (set profile)
    let profileIdField = "quality_profile_id"
    maybeProfileId <- if profileIdField `KM.member` o
      then Just <$> o .: profileIdField
      else pure Nothing
    pure $ UpdateCatalogAlbumRequest maybeProfileId

-- | Request to create a catalog task.
data CatalogTaskRequest = CatalogTaskRequest
  { catalogTaskType :: Text
    -- ^ Task type: "refresh" or "refresh_all"
  , catalogTaskArtistId :: Maybe Int64
    -- ^ Artist ID to refresh (Nothing for refresh_all)
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogTaskRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

instance FromJSON CatalogTaskRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

-- ============================================================================
-- ENHANCED CATALOG OVERVIEW
-- ============================================================================

-- | Album state (computed from catalog + downloads + library).
-- AlbumState is now imported from Skema.Core.Catalog

-- | Active download information.
data ActiveDownloadInfo = ActiveDownloadInfo
  { activeDownloadId :: Int64
  , activeDownloadStatus :: Text
  , activeDownloadProgress :: Double
  , activeDownloadQuality :: Maybe Text
  , activeDownloadTitle :: Text
  , activeDownloadSizeBytes :: Maybe Int64
  , activeDownloadStartedAt :: Maybe Text
  , activeDownloadErrorMessage :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ActiveDownloadInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 14 }

instance FromJSON ActiveDownloadInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 14 }

-- | Enhanced catalog album response with state and download info.
data CatalogAlbumOverviewResponse = CatalogAlbumOverviewResponse
  { catalogAlbumOverviewId :: Int64
  , catalogAlbumOverviewReleaseGroupMBID :: Text
  , catalogAlbumOverviewTitle :: Text
  , catalogAlbumOverviewArtistId :: Maybe Int64
  , catalogAlbumOverviewArtistMBID :: Text
  , catalogAlbumOverviewArtistName :: Text
  , catalogAlbumOverviewType :: Maybe Text
  , catalogAlbumOverviewFirstReleaseDate :: Maybe Text
  , catalogAlbumOverviewCoverUrl :: Maybe Text
  , catalogAlbumOverviewCoverThumbnailUrl :: Maybe Text

  -- State information
  , catalogAlbumOverviewState :: AlbumState
  , catalogAlbumOverviewWanted :: Bool
  , catalogAlbumOverviewHasCluster :: Bool
  , catalogAlbumOverviewMatchedClusterId :: Maybe Int64
  , catalogAlbumOverviewCurrentQuality :: Maybe Text
  , catalogAlbumOverviewQualityProfileId :: Maybe Int64
  , catalogAlbumOverviewQualityProfileName :: Maybe Text

  -- Download information
  , catalogAlbumOverviewActiveDownload :: Maybe ActiveDownloadInfo
  , catalogAlbumOverviewDownloadCount :: Int
  , catalogAlbumOverviewLastDownloadAt :: Maybe Text

  -- Timestamps
  , catalogAlbumOverviewCreatedAt :: Text
  , catalogAlbumOverviewUpdatedAt :: Text
  , catalogAlbumOverviewImportedAt :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogAlbumOverviewResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

instance FromJSON CatalogAlbumOverviewResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

-- | Request for catalog albums overview (with filtering, sorting, pagination).
data CatalogAlbumOverviewRequest = CatalogAlbumOverviewRequest
  { catalogAlbumOverviewReqPage :: Maybe Int
  , catalogAlbumOverviewReqLimit :: Maybe Int
  , catalogAlbumOverviewReqState :: Maybe [AlbumState]
  , catalogAlbumOverviewReqQuality :: Maybe [Text]
  , catalogAlbumOverviewReqArtistId :: Maybe Int64
  , catalogAlbumOverviewReqSearch :: Maybe Text
  , catalogAlbumOverviewReqSort :: Maybe Text  -- ^ title, artist, date, quality, state
  } deriving (Show, Eq, Generic)

instance ToJSON CatalogAlbumOverviewRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

instance FromJSON CatalogAlbumOverviewRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

-- | Pagination info.
data AlbumOverviewPagination = AlbumOverviewPagination
  { albumOverviewPaginationTotal :: Int
  , albumOverviewPaginationOffset :: Int
  , albumOverviewPaginationLimit :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON AlbumOverviewPagination where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

instance FromJSON AlbumOverviewPagination where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

-- | Album statistics.
data AlbumOverviewStats = AlbumOverviewStats
  { albumOverviewStatsByState :: [(AlbumState, Int)]
  , albumOverviewStatsByQuality :: [(Text, Int)]
  } deriving (Show, Eq, Generic)

instance ToJSON AlbumOverviewStats where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

instance FromJSON AlbumOverviewStats where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 18 }

-- | Complete album overview response (wrapper for pagination, stats, and albums).
data AlbumOverviewResponse = AlbumOverviewResponse
  { albumOverviewResponsePagination :: AlbumOverviewPagination
  , albumOverviewResponseStats :: AlbumOverviewStats
  , albumOverviewResponseAlbums :: [CatalogAlbumOverviewResponse]
  } deriving (Show, Eq, Generic)

instance ToJSON AlbumOverviewResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance FromJSON AlbumOverviewResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

-- | Bulk action type.
data BulkAlbumAction
  = SetQualityProfile Int64
  | SetWanted Bool
  | TriggerSearch
  | DeleteFromCatalog
  deriving (Show, Eq, Generic)

instance ToJSON BulkAlbumAction
instance FromJSON BulkAlbumAction

-- | Bulk operation request.
data BulkAlbumActionRequest = BulkAlbumActionRequest
  { bulkAlbumActionAlbumIds :: [Int64]
  , bulkAlbumActionAction :: BulkAlbumAction
  } deriving (Show, Eq, Generic)

instance ToJSON BulkAlbumActionRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON BulkAlbumActionRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

-- ============================================================================
-- ALBUM RELEASES SEARCH
-- ============================================================================

-- | Response for available releases search
data AlbumReleasesResponse = AlbumReleasesResponse
  { albumReleasesAlbum :: CatalogAlbumResponse
  , albumReleasesReleases :: [ReleaseResponse]
  , albumReleasesSearchTime :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON AlbumReleasesResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

instance FromJSON AlbumReleasesResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

-- | Individual release information
data ReleaseResponse = ReleaseResponse
  { releaseResponseTitle :: Text
  , releaseResponseSource :: Text  -- Indexer name
  , releaseResponseQuality :: Text  -- FLAC, MP3 320, etc.
  , releaseResponseSize :: Maybe Int64
  , releaseResponseSeeders :: Maybe Int
  , releaseResponsePeers :: Maybe Int
  , releaseResponseDownloadType :: Text  -- "nzb", "torrent", or "slskd"
  , releaseResponseDownloadUrl :: Text
  , releaseResponsePublishDate :: Maybe Text
  , releaseResponseSlskdUsername :: Maybe Text  -- For slskd downloads
  , releaseResponseSlskdFiles :: Maybe [SlskdFileResponse]  -- For slskd downloads
  } deriving (Show, Eq, Generic)

-- | Slskd file information for downloads
data SlskdFileResponse = SlskdFileResponse
  { slskdFileFilename :: Text
  , slskdFileSize :: Integer
  } deriving (Show, Eq, Generic)

instance ToJSON SlskdFileResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9 }

instance FromJSON SlskdFileResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9 }

instance ToJSON ReleaseResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON ReleaseResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

-- ============================================================================
-- STREAMING RELEASE SEARCH
-- ============================================================================

-- | Events for streaming release search results
data ReleaseStreamEvent
  = ReleaseFound ReleaseResponse Text  -- Release and source name
  | SearchStarted Text                 -- Source name starting search
  | SearchCompleted Text Int           -- Source name and result count
  | SearchError Text Text              -- Source name and error message
  | SearchDone Double                  -- Total search time
  deriving (Show, Eq, Generic)

instance ToJSON ReleaseStreamEvent where
  toJSON (ReleaseFound release source) = object
    [ "type" .= ("release" :: Text)
    , "source" .= source
    , "release" .= release
    ]
  toJSON (SearchStarted source) = object
    [ "type" .= ("started" :: Text)
    , "source" .= source
    ]
  toJSON (SearchCompleted source count) = object
    [ "type" .= ("completed" :: Text)
    , "source" .= source
    , "count" .= count
    ]
  toJSON (SearchError source err) = object
    [ "type" .= ("error" :: Text)
    , "source" .= source
    , "error" .= err
    ]
  toJSON (SearchDone totalTime) = object
    [ "type" .= ("done" :: Text)
    , "total_time" .= totalTime
    ]

-- | ToServerEvent instance for servant-event-stream
instance ToServerEvent ReleaseStreamEvent where
  toServerEvent evt =
    let jsonData = encode evt
        evtType = case evt of
          ReleaseFound {} -> "release"
          SearchStarted {} -> "started"
          SearchCompleted {} -> "completed"
          SearchError {} -> "error"
          SearchDone {} -> "done"
    in SSE.ServerEvent
         { SSE.eventType = Just (BSL.fromStrict $ TE.encodeUtf8 evtType)
         , SSE.eventId = Nothing
         , SSE.eventData = jsonData
         }
