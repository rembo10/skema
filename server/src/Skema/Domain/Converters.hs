{-# LANGUAGE OverloadedStrings #-}

-- | Pure conversion functions between domain types and API types.
--
-- This module contains all pure data transformations with no side effects.
-- All functions are fully testable without mocking.
module Skema.Domain.Converters
  ( -- * Config conversions
    downloadClientTypeToAPI
  , downloadClientToAPI
  , downloadClientTypeFromAPI
  , downloadClientFromAPI
  , indexerToAPI
  , indexerFromAPI
  , musicBrainzServerToText
  , textToMusicBrainzServer
  , configToResponse
    -- * Cluster conversions
  , clusterToResponse
    -- * MusicBrainz search conversions
  , mbArtistSearchToCatalogResponse
  , mbReleaseGroupSearchToCatalogResponse
    -- * Download conversions
  , downloadRecordToResponse
  ) where

import Skema.API.Types.Config (DownloadClientType(..), DownloadClientResponse(..), IndexerResponse(..), ConfigResponse(..))
import Skema.API.Types.Clusters (ClusterResponse(..))
import Skema.API.Types.Catalog (CatalogArtistResponse(..), CatalogAlbumResponse(..))
import Skema.API.Types.Downloads (DownloadResponse(..))
import qualified Skema.Config.Types as Cfg
import qualified Skema.Config.Validation as CfgVal
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Utils as DBUtils
import Skema.MusicBrainz.Types
import qualified System.OsPath as OP
import Data.Aeson (eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Time (UTCTime)

-- * Config Conversions

-- | Convert download client type from Config to API.
downloadClientTypeToAPI :: Cfg.DownloadClientType -> DownloadClientType
downloadClientTypeToAPI Cfg.SABnzbd = DCTypeSABnzbd
downloadClientTypeToAPI Cfg.NZBGet = DCTypeNZBGet
downloadClientTypeToAPI Cfg.Transmission = DCTypeTransmission
downloadClientTypeToAPI Cfg.QBittorrent = DCTypeQBittorrent

-- | Convert download client from Config to API.
downloadClientToAPI :: Cfg.DownloadClient -> DownloadClientResponse
downloadClientToAPI dc = DownloadClientResponse
  { dcResponseType = downloadClientTypeToAPI (Cfg.dcType dc)
  , dcResponseUrl = Cfg.dcUrl dc
  , dcResponseApiKey = Cfg.dcApiKey dc
  , dcResponseUsername = Cfg.dcUsername dc
  , dcResponsePassword = Cfg.dcPassword dc
  , dcResponseEnabled = Cfg.dcEnabled dc
  , dcResponseDownloadDir = Cfg.dcDownloadDir dc
  , dcResponseCategory = Cfg.dcCategory dc
  }

-- | Convert download client type from API to Config.
downloadClientTypeFromAPI :: DownloadClientType -> Cfg.DownloadClientType
downloadClientTypeFromAPI DCTypeSABnzbd = Cfg.SABnzbd
downloadClientTypeFromAPI DCTypeNZBGet = Cfg.NZBGet
downloadClientTypeFromAPI DCTypeTransmission = Cfg.Transmission
downloadClientTypeFromAPI DCTypeQBittorrent = Cfg.QBittorrent

-- | Convert download client from API to Config.
downloadClientFromAPI :: DownloadClientResponse -> Cfg.DownloadClient
downloadClientFromAPI dc = Cfg.DownloadClient
  { Cfg.dcType = downloadClientTypeFromAPI (dcResponseType dc)
  , Cfg.dcUrl = dcResponseUrl dc
  , Cfg.dcApiKey = dcResponseApiKey dc
  , Cfg.dcUsername = dcResponseUsername dc
  , Cfg.dcPassword = dcResponsePassword dc
  , Cfg.dcEnabled = dcResponseEnabled dc
  , Cfg.dcDownloadDir = dcResponseDownloadDir dc
  , Cfg.dcCategory = dcResponseCategory dc
  }

-- | Convert indexer from Config to API.
indexerToAPI :: Cfg.Indexer -> IndexerResponse
indexerToAPI idx = IndexerResponse
  { indexerResponseName = Cfg.indexerName idx
  , indexerResponseUrl = Cfg.indexerUrl idx
  , indexerResponseApiKey = Cfg.indexerApiKey idx
  , indexerResponseUsername = Cfg.indexerUsername idx
  , indexerResponsePassword = Cfg.indexerPassword idx
  , indexerResponseEnabled = Cfg.indexerEnabled idx
  , indexerResponsePriority = Cfg.indexerPriority idx
  , indexerResponseCategories = Cfg.indexerCategories idx
  }

-- | Convert indexer from API to Config.
indexerFromAPI :: IndexerResponse -> Cfg.Indexer
indexerFromAPI idx = Cfg.Indexer
  { Cfg.indexerName = indexerResponseName idx
  , Cfg.indexerUrl = indexerResponseUrl idx
  , Cfg.indexerApiKey = indexerResponseApiKey idx
  , Cfg.indexerUsername = indexerResponseUsername idx
  , Cfg.indexerPassword = indexerResponsePassword idx
  , Cfg.indexerEnabled = indexerResponseEnabled idx
  , Cfg.indexerPriority = indexerResponsePriority idx
  , Cfg.indexerCategories = indexerResponseCategories idx
  }

-- | Convert MusicBrainz server to Text.
musicBrainzServerToText :: Cfg.MusicBrainzServer -> Text
musicBrainzServerToText Cfg.OfficialMusicBrainz = "official"
musicBrainzServerToText Cfg.HeadphonesVIP = "headphones_vip"

-- | Convert Text to MusicBrainz server.
textToMusicBrainzServer :: Text -> Maybe Cfg.MusicBrainzServer
textToMusicBrainzServer "official" = Just Cfg.OfficialMusicBrainz
textToMusicBrainzServer "headphones_vip" = Just Cfg.HeadphonesVIP
textToMusicBrainzServer _ = Nothing

-- | Convert Config to ConfigResponse for API (requires IO for path conversion).
--
-- Note: This function requires IO for OsPath decoding and checking env var overrides.
-- The actual business logic is pure.
configToResponse :: Cfg.Config -> IO ConfigResponse
configToResponse cfg = do
  let libCfg = Cfg.library cfg
  let sysCfg = Cfg.system cfg
  let srvCfg = Cfg.server cfg
  let dlCfg = Cfg.download cfg
  let idxCfg = Cfg.indexers cfg
  let mbCfg = Cfg.musicbrainz cfg

  -- Convert library path from OsPath to Text
  libPath <- case Cfg.libraryPath libCfg of
    Nothing -> pure Nothing
    Just osPath -> Just . toText <$> OP.decodeUtf osPath

  -- Check if auth is enabled (with environment variable overrides)
  -- This gives the ACTUAL auth state, not just config file values
  maybeAuthCreds <- CfgVal.getAuthCredentials srvCfg
  let authEnabled = isJust maybeAuthCreds

  -- Get the effective username (with env var override if present)
  -- This shows what username is ACTUALLY being used for authentication
  let effectiveUsername = case maybeAuthCreds of
        Just (username, _) -> Just username
        Nothing -> Cfg.serverUsername srvCfg  -- Fall back to config file value if auth disabled

  pure $ ConfigResponse
    { configLibraryPath = libPath
    , configLibraryWatch = Cfg.libraryWatch libCfg
    , configLibraryAutoScan = Cfg.libraryAutoScan libCfg
    , configLibraryAutoScanIntervalMins = Cfg.libraryAutoScanIntervalMins libCfg
    , configLibraryAutoScanOnStartup = Cfg.libraryAutoScanOnStartup libCfg
    , configLibraryNormalizeFeaturing = Cfg.libraryNormalizeFeaturing libCfg
    , configLibraryNormalizeFeaturingTo = Cfg.libraryNormalizeFeaturingTo libCfg
    , configLibraryPathFormat = Cfg.libraryPathFormat libCfg
    , configLibraryFileFormat = Cfg.libraryFileFormat libCfg
    , configSystemWatchConfigFile = Cfg.systemWatchConfigFile sysCfg
    , configSystemDatabaseBackend = Cfg.systemDatabaseBackend sysCfg
    , configSystemDatabasePath = Cfg.systemDatabasePath sysCfg
    , configServerHost = Cfg.serverHost srvCfg
    , configServerPort = Cfg.serverPort srvCfg
    , configServerUsername = effectiveUsername  -- Use effective username (with env var override)
    , configServerJwtExpirationHours = Cfg.serverJwtExpirationHours srvCfg
    , configServerAuthEnabled = authEnabled
    , configDownloadNzbClient = fmap downloadClientToAPI (Cfg.downloadNzbClient dlCfg)
    , configDownloadTorrentClient = fmap downloadClientToAPI (Cfg.downloadTorrentClient dlCfg)
    , configDownloadDirectory = Cfg.downloadDirectory dlCfg
    , configDownloadCheckInterval = Cfg.downloadCheckInterval dlCfg
    , configDownloadAutoImport = Cfg.downloadAutoImport dlCfg
    , configDownloadMinSeeders = Cfg.downloadMinSeeders dlCfg
    , configDownloadMaxSizeMB = Cfg.downloadMaxSize dlCfg
    , configIndexersList = map indexerToAPI (Cfg.indexerList idxCfg)
    , configIndexersSearchTimeout = Cfg.indexerSearchTimeout idxCfg
    , configMusicBrainzServer = musicBrainzServerToText (Cfg.mbServer mbCfg)
    , configMusicBrainzUsername = Cfg.mbUsername mbCfg
    , configMusicBrainzPassword = Cfg.mbPassword mbCfg
    }

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
    , catalogAlbumResponseWanted = maybe False DBTypes.catalogAlbumWanted maybeCatalogRecord
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
