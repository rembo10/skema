{-# LANGUAGE OverloadedStrings #-}

-- | Pure conversion functions between domain types and API types.
--
-- This module contains all pure data transformations with no side effects.
-- All functions are fully testable without mocking.
--
-- Note: Config conversions are now handled by ConfigJSON module using JSON merge.
module Skema.Domain.Converters
  ( -- * Cluster conversions
    clusterToResponse
    -- * MusicBrainz search conversions
  , mbArtistSearchToCatalogResponse
  , mbReleaseGroupSearchToCatalogResponse
    -- * Download conversions
  , downloadRecordToResponse
  ) where

import Skema.API.Types.Clusters (ClusterResponse(..))
import Skema.API.Types.Catalog (CatalogArtistResponse(..), CatalogAlbumResponse(..))
import Skema.API.Types.Downloads (DownloadResponse(..))
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Utils as DBUtils
import Skema.MusicBrainz.Types
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime)

-- * Cluster Conversions

-- | Convert ClusterRecord to ClusterResponse.
--
-- Release details are cached as JSON in the database.
-- Takes optional metadata from first track to include identification data.
clusterToResponse :: DBTypes.ClusterRecord -> Maybe DBTypes.LibraryTrackMetadataRecord -> ClusterResponse
clusterToResponse cluster maybeMetadata =
  let
    -- Deserialize the cached MB release data from JSON
    mbRelease = case DBTypes.clusterMBReleaseData cluster of
      Nothing -> Nothing
      Just jsonText -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
        Left _ -> Nothing  -- JSON parse failed, treat as no data
        Right release -> Just (release :: MBRelease)
  in
    ClusterResponse
      { clusterResponseId = fromMaybe 0 (DBTypes.clusterId cluster)
      , clusterResponseMetadataHash = DBTypes.clusterMetadataHash cluster
      , clusterResponseAlbum = DBTypes.clusterAlbum cluster
      , clusterResponseAlbumArtist = DBTypes.clusterAlbumArtist cluster
      , clusterResponseTrackCount = DBTypes.clusterTrackCount cluster
      , clusterResponseMBReleaseId = DBTypes.clusterMBReleaseId cluster
      , clusterResponseMBReleaseGroupId = DBTypes.clusterMBReleaseGroupId cluster
      , clusterResponseMBConfidence = DBTypes.clusterMBConfidence cluster
      , clusterResponseCreatedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.clusterCreatedAt cluster)
      , clusterResponseUpdatedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.clusterUpdatedAt cluster)
      , clusterResponseLastIdentifiedAt = fmap (show :: UTCTime -> Text) (DBTypes.clusterLastIdentifiedAt cluster)
      -- Match provenance
      , clusterResponseMatchSource = fmap DBTypes.matchSourceToText (DBTypes.clusterMatchSource cluster)
      , clusterResponseMatchLocked = DBTypes.clusterMatchLocked cluster
      -- Cluster metadata from first track (used for MusicBrainz identification)
      , clusterResponseLabel = maybeMetadata >>= DBTypes.metaLabel
      , clusterResponseCatalogNumber = maybeMetadata >>= DBTypes.metaCatalogNumber
      , clusterResponseBarcode = maybeMetadata >>= DBTypes.metaBarcode
      , clusterResponseCountry = maybeMetadata >>= DBTypes.metaCountry
      , clusterResponseDate = maybeMetadata >>= DBTypes.metaDate
      , clusterResponseYear = maybeMetadata >>= DBTypes.metaYear
      -- MusicBrainz release details (deserialized from cached JSON)
      , clusterResponseMBReleaseTitle = mbReleaseTitle <$> mbRelease
      , clusterResponseMBReleaseArtist = mbReleaseArtist <$> mbRelease
      , clusterResponseMBReleaseDate = mbRelease >>= mbReleaseDate
      , clusterResponseMBReleaseCountry = mbRelease >>= mbReleaseCountry
      , clusterResponseMBReleaseLabel = mbRelease >>= mbReleaseLabel
      , clusterResponseMBReleaseCatalogNumber = mbRelease >>= mbReleaseCatalogNumber
      , clusterResponseMBReleaseBarcode = mbRelease >>= mbReleaseBarcode
      -- Alternative match candidates (JSON array)
      , clusterResponseMBCandidates = DBTypes.clusterMBCandidates cluster
      }

-- * MusicBrainz Search Conversions

-- | Convert MusicBrainz artist search result to catalog response.
mbArtistSearchToCatalogResponse :: MBArtistSearchResult -> Maybe DBTypes.CatalogArtistRecord -> CatalogArtistResponse
mbArtistSearchToCatalogResponse searchResult maybeCatalogRecord =
  CatalogArtistResponse
    { catalogArtistResponseId = maybeCatalogRecord >>= DBTypes.catalogArtistId
    , catalogArtistResponseMBID = unMBID (mbasArtistId searchResult)
    , catalogArtistResponseName = mbasArtistName searchResult
    , catalogArtistResponseType = mbasArtistType searchResult
    , catalogArtistResponseImageUrl = maybeCatalogRecord >>= DBTypes.catalogArtistImageUrl
    , catalogArtistResponseThumbnailUrl = maybeCatalogRecord >>= DBTypes.catalogArtistThumbnailUrl
    , catalogArtistResponseFollowed = maybe False DBTypes.catalogArtistFollowed maybeCatalogRecord
    , catalogArtistResponseQualityProfileId = maybeCatalogRecord >>= DBTypes.catalogArtistQualityProfileId
    , catalogArtistResponseScore = Just (mbasScore searchResult)
    , catalogArtistResponseCreatedAt = maybeCatalogRecord >>= DBTypes.catalogArtistCreatedAt >>= (Just . show)
    , catalogArtistResponseUpdatedAt = maybeCatalogRecord >>= DBTypes.catalogArtistUpdatedAt >>= (Just . show)
    , catalogArtistResponseAlbums = Nothing  -- Not needed for search results
    }

-- | Convert MusicBrainz release-group search result to catalog response.
mbReleaseGroupSearchToCatalogResponse :: MBReleaseGroupSearchResult -> Maybe DBTypes.CatalogAlbumRecord -> CatalogAlbumResponse
mbReleaseGroupSearchToCatalogResponse searchResult maybeCatalogRecord =
  CatalogAlbumResponse
    { catalogAlbumResponseId = maybeCatalogRecord >>= DBTypes.catalogAlbumId
    , catalogAlbumResponseReleaseGroupMBID = unMBID (mbrgsReleaseGroupId searchResult)
    , catalogAlbumResponseTitle = mbrgsTitle searchResult
    , catalogAlbumResponseArtistMBID = maybe "" unMBID (mbrgsArtistId searchResult)
    , catalogAlbumResponseArtistName = mbrgsArtistName searchResult
    , catalogAlbumResponseType = mbrgsType searchResult
    , catalogAlbumResponseFirstReleaseDate = mbrgsFirstReleaseDate searchResult
    , catalogAlbumResponseCoverUrl = maybeCatalogRecord >>= DBTypes.catalogAlbumCoverUrl
    , catalogAlbumResponseCoverThumbnailUrl = maybeCatalogRecord >>= DBTypes.catalogAlbumCoverThumbnailUrl
    -- NOTE: "wanted" is now computed from quality profile, not stored
    -- For MusicBrainz search results, we default to False (not in catalog yet)
    , catalogAlbumResponseWanted = False
    , catalogAlbumResponseMatchedClusterId = maybeCatalogRecord >>= DBTypes.catalogAlbumMatchedClusterId
    , catalogAlbumResponseQualityProfileId = maybeCatalogRecord >>= DBTypes.catalogAlbumQualityProfileId
    , catalogAlbumResponseScore = Just (mbrgsScore searchResult)
    , catalogAlbumResponseCreatedAt = maybeCatalogRecord >>= DBTypes.catalogAlbumCreatedAt >>= (Just . show)
    , catalogAlbumResponseUpdatedAt = maybeCatalogRecord >>= DBTypes.catalogAlbumUpdatedAt >>= (Just . show)
    }

-- * Download Conversions

-- | Convert DownloadRecord to DownloadResponse.
downloadRecordToResponse :: DBTypes.DownloadRecord -> DownloadResponse
downloadRecordToResponse download =
  DownloadResponse
    { downloadResponseId = fromMaybe 0 (DBTypes.downloadId download)
    , downloadResponseCatalogAlbumId = DBTypes.downloadCatalogAlbumId download
    , downloadResponseIndexerName = DBTypes.downloadIndexerName download
    , downloadResponseDownloadUrl = DBTypes.downloadUrl download
    , downloadResponseDownloadClient = DBTypes.downloadClient download
    , downloadResponseDownloadClientId = DBTypes.downloadClientId download
    , downloadResponseStatus = DBUtils.downloadStatusToText (DBTypes.downloadStatus download)
    , downloadResponseDownloadPath = DBTypes.downloadPath download
    , downloadResponseTitle = DBTypes.downloadTitle download
    , downloadResponseSizeBytes = DBTypes.downloadSizeBytes download
    , downloadResponseQuality = DBTypes.downloadQuality download
    , downloadResponseFormat = DBTypes.downloadFormat download
    , downloadResponseSeeders = DBTypes.downloadSeeders download
    , downloadResponseProgress = DBTypes.downloadProgress download
    , downloadResponseErrorMessage = DBTypes.downloadErrorMessage download
    , downloadResponseQueuedAt = fmap (show :: UTCTime -> Text) (DBTypes.downloadQueuedAt download)
    , downloadResponseStartedAt = fmap (show :: UTCTime -> Text) (DBTypes.downloadStartedAt download)
    , downloadResponseCompletedAt = fmap (show :: UTCTime -> Text) (DBTypes.downloadCompletedAt download)
    , downloadResponseImportedAt = fmap (show :: UTCTime -> Text) (DBTypes.downloadImportedAt download)
    , downloadResponseLibraryPath = DBTypes.downloadLibraryPath download
    }
