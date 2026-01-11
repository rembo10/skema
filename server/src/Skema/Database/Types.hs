{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Database types and configuration (SQLite only).
module Skema.Database.Types
  ( -- * Database Configuration
    DatabaseConfig (..)
    -- * Database Models
  , LibraryTrackRecord (..)
  , LibraryTrackMetadataRecord (..)
  , ScanHistoryRecord (..)
  , MetadataDiffRecord (..)
  , MetadataChangeRecord (..)
  , ClusterRecord (..)
    -- * Acquisition Models
  , AcquisitionSourceRecord (..)
  , TrackedArtistRecord (..)
  , WantedAlbumRecord (..)
  , SourceType (..)
  , AlbumStatus (..)
  , sourceTypeToText
  , textToSourceType
  , albumStatusToText
  , textToAlbumStatus
  , matchSourceToText
  , textToMatchSource
  , MatchSource(..)
    -- * Catalog Models
  , CatalogArtistRecord (..)
  , CatalogAlbumRecord (..)
    -- * Download Models
  , DownloadRecord (..)
  , DownloadStatus (..)
  , downloadStatusToText
  , textToDownloadStatus
    -- * Quality Profile Models
  , QualityProfileRecord (..)
    -- * Constants
  , variousArtistsMBID
  , isVariousArtists
  ) where

import GHC.Generics ()
import Data.Data (Data, toConstr, showConstr)
import Data.Char (isUpper, isLower, toLower)
import Data.Time (UTCTime)
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.FromRow as SQLite
import System.IO.Unsafe (unsafePerformIO)

-- * Constants

-- | MusicBrainz ID for "Various Artists" - a special artist used for compilations.
-- We exclude this from catalog_artists as it's not a trackable artist.
variousArtistsMBID :: Text
variousArtistsMBID = "89ad4ac3-39f7-470e-963a-56509c546377"

-- | Check if an artist MBID is "Various Artists".
isVariousArtists :: Text -> Bool
isVariousArtists mbid = mbid == variousArtistsMBID

-- * Database Types

-- | Database configuration.
data DatabaseConfig = DatabaseConfig
  { dbPath :: FilePath
    -- ^ Path to SQLite database file
  , dbPoolSize :: Int
    -- ^ Maximum number of connections in pool
  } deriving (Show, Eq, Generic)

-- | Library track record stored in database.
data LibraryTrackRecord = LibraryTrackRecord
  { trackId :: Maybe Int64
  , trackPath :: OsPath
  , trackSize :: Integer
  , trackModifiedAt :: UTCTime
  , trackClusterId :: Maybe Int64
  , trackMBRecordingId :: Maybe Text
  , trackMBTrackId :: Maybe Text
  , trackMBReleaseId :: Maybe Text
  , trackMBConfidence :: Maybe Double
  , trackAddedAt :: Maybe UTCTime
  , trackUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Library track metadata record - reflects actual file tags.
data LibraryTrackMetadataRecord = LibraryTrackMetadataRecord
  { metaTrackId :: Maybe Int64
  , metaFormat :: Maybe Text  -- Audio format: "FLAC", "MP3", "OGG", "Opus"
  , metaTitle :: Maybe Text
  , metaArtist :: Maybe Text
  , metaAlbum :: Maybe Text
  , metaAlbumArtist :: Maybe Text
  , metaTrackNumber :: Maybe Int
  , metaTotalTracks :: Maybe Int
  , metaDiscNumber :: Maybe Int
  , metaTotalDiscs :: Maybe Int
  , metaDate :: Maybe Text
  , metaYear :: Maybe Int
  , metaGenre :: Maybe Text
  , metaPublisher :: Maybe Text
  , metaComment :: Maybe Text
  , metaDurationSeconds :: Maybe Double
  , metaBitsPerSample :: Maybe Int
  , metaCountry :: Maybe Text  -- Maps to "country" column in database
  , metaLabel :: Maybe Text  -- Maps to "label" column in database
  , metaCatalogNumber :: Maybe Text
  , metaBarcode :: Maybe Text
  , metaReleaseStatus :: Maybe Text
  , metaReleaseType :: Maybe Text
  , metaMBRecordingId :: Maybe Text
  , metaMBTrackId :: Maybe Text
  , metaMBReleaseId :: Maybe Text
  , metaMBReleaseGroupId :: Maybe Text
  , metaMBArtistId :: Maybe Text
  , metaMBAlbumArtistId :: Maybe Text
  , metaMBWorkId :: Maybe Text
  , metaMBDiscId :: Maybe Text
  , metaAcoustidFingerprint :: Maybe Text
  , metaAcoustidId :: Maybe Text
  , metaCreatedAt :: Maybe UTCTime
  , metaUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Scan history record.
data ScanHistoryRecord = ScanHistoryRecord
  { scanId :: Maybe Int64
  , scanStartedAt :: UTCTime
  , scanCompletedAt :: Maybe UTCTime
  , scanFilesAdded :: Int
  , scanFilesModified :: Int
  , scanFilesDeleted :: Int
  , scanLibraryAvailable :: Bool
  , scanError :: Maybe Text
  } deriving (Show, Eq, Generic)

-- | Metadata diff record.
data MetadataDiffRecord = MetadataDiffRecord
  { diffId :: Maybe Int64
  , diffTrackId :: Int64
  , diffFieldName :: Text
  , diffFileValue :: Maybe Text
  , diffMBValue :: Maybe Text
  , diffCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Metadata change record.
data MetadataChangeRecord = MetadataChangeRecord
  { changeId :: Maybe Int64
  , changeTrackId :: Int64
  , changeFieldName :: Text
  , changeOldValue :: Maybe Text
  , changeNewValue :: Maybe Text
  , changeAppliedAt :: Maybe UTCTime
  , changeRevertedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Match source enumeration - how was this cluster matched to MB?
data MatchSource
  = AutoFingerprint  -- Matched automatically via AcoustID fingerprint
  | AutoMetadata     -- Matched automatically via metadata search
  | Manual           -- Manually assigned by user
  deriving (Show, Eq, Generic, Data)

-- | Cluster record.
data ClusterRecord = ClusterRecord
  { clusterId :: Maybe Int64
  , clusterMetadataHash :: Text
  , clusterAlbum :: Maybe Text
  , clusterAlbumArtist :: Maybe Text
  , clusterTrackCount :: Int
  , clusterMBReleaseId :: Maybe Text
  , clusterMBReleaseGroupId :: Maybe Text
  , clusterMBConfidence :: Maybe Double
  , clusterCreatedAt :: Maybe UTCTime
  , clusterUpdatedAt :: Maybe UTCTime
  , clusterLastIdentifiedAt :: Maybe UTCTime
  , clusterMBReleaseData :: Maybe Text
  , clusterMBCandidates :: Maybe Text
  , clusterMatchSource :: Maybe MatchSource
  , clusterMatchLocked :: Bool
  } deriving (Show, Eq, Generic)

-- | Source type for acquisition sources (providers).
data SourceType
  = LibraryArtists
  | Metacritic
  | Pitchfork
  deriving (Show, Eq, Generic, Data)

-- | Album status for wanted albums.
data AlbumStatus
  = Wanted
  | Upgrading
  | Acquired
  | Ignored
  | Monitoring
  deriving (Show, Eq, Generic, Data)

-- | Acquisition source record (stored in acquisition_rules table for backward compatibility).
data AcquisitionSourceRecord = AcquisitionSourceRecord
  { sourceId :: Maybe Int64
  , sourceName :: Text
  , sourceDescription :: Maybe Text
  , sourceType :: SourceType
  , sourceEnabled :: Bool
  , sourceFilters :: Maybe Text  -- Provider-specific JSON
  , sourceCreatedAt :: Maybe UTCTime
  , sourceUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Tracked artist record.
data TrackedArtistRecord = TrackedArtistRecord
  { trackedArtistId :: Maybe Int64
  , trackedArtistMBID :: Text
  , trackedArtistName :: Text
  , trackedArtistImageUrl :: Maybe Text
  , trackedArtistThumbnailUrl :: Maybe Text
  , trackedArtistAddedBySourceId :: Int64
  , trackedArtistSourceClusterId :: Maybe Int64
  , trackedArtistLastCheckedAt :: Maybe UTCTime
  , trackedArtistCreatedAt :: Maybe UTCTime
  , trackedArtistUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Wanted album record.
data WantedAlbumRecord = WantedAlbumRecord
  { wantedAlbumId :: Maybe Int64
  , wantedAlbumReleaseGroupMBID :: Text
  , wantedAlbumTitle :: Text
  , wantedAlbumArtistMBID :: Text
  , wantedAlbumArtistName :: Text
  , wantedAlbumStatus :: AlbumStatus
  , wantedAlbumAddedBySourceId :: Int64
  , wantedAlbumFirstReleaseDate :: Maybe Text
  , wantedAlbumMatchedClusterId :: Maybe Int64
  , wantedAlbumCreatedAt :: Maybe UTCTime
  , wantedAlbumUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Catalog artist record.
data CatalogArtistRecord = CatalogArtistRecord
  { catalogArtistId :: Maybe Int64
  , catalogArtistMBID :: Text
  , catalogArtistName :: Text
  , catalogArtistType :: Maybe Text
  , catalogArtistImageUrl :: Maybe Text
  , catalogArtistThumbnailUrl :: Maybe Text
  , catalogArtistFollowed :: Bool
  , catalogArtistAddedBySourceId :: Maybe Int64
  , catalogArtistSourceClusterId :: Maybe Int64
  , catalogArtistLastCheckedAt :: Maybe UTCTime
  , catalogArtistQualityProfileId :: Maybe Int64
  , catalogArtistCreatedAt :: Maybe UTCTime
  , catalogArtistUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Catalog album record.
-- NOTE: "wanted" status is NOT stored - it's computed from quality_profile_id + current_quality + matched_cluster_id
data CatalogAlbumRecord = CatalogAlbumRecord
  { catalogAlbumId :: Maybe Int64
  , catalogAlbumReleaseGroupMBID :: Text
  , catalogAlbumTitle :: Text
  , catalogAlbumArtistId :: Maybe Int64
  , catalogAlbumArtistMBID :: Text
  , catalogAlbumArtistName :: Text
  , catalogAlbumType :: Maybe Text
  , catalogAlbumFirstReleaseDate :: Maybe Text
  , catalogAlbumCoverUrl :: Maybe Text
  , catalogAlbumCoverThumbnailUrl :: Maybe Text
  , catalogAlbumMatchedClusterId :: Maybe Int64
  , catalogAlbumQualityProfileId :: Maybe Int64
  , catalogAlbumCurrentQuality :: Maybe Text
  , catalogAlbumCreatedAt :: Maybe UTCTime
  , catalogAlbumUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Download status enumeration.
data DownloadStatus
  = DownloadQueued
  | DownloadDownloading
  | DownloadCompleted
  | DownloadFailed
  | DownloadImported
  | DownloadCancelled
  | DownloadIdentificationFailure
  deriving (Show, Eq, Generic, Data)

-- | Download record.
data DownloadRecord = DownloadRecord
  { downloadId :: Maybe Int64
  , downloadCatalogAlbumId :: Int64
  , downloadIndexerName :: Text
  , downloadUrl :: Text
  , downloadClient :: Maybe Text
  , downloadClientId :: Maybe Text
  , downloadStatus :: DownloadStatus
  , downloadPath :: Maybe Text
  , downloadTitle :: Text
  , downloadSizeBytes :: Maybe Int64
  , downloadQuality :: Maybe Text
  , downloadFormat :: Maybe Text
  , downloadSeeders :: Maybe Int
  , downloadProgress :: Double
  , downloadErrorMessage :: Maybe Text
  , downloadQueuedAt :: Maybe UTCTime
  , downloadStartedAt :: Maybe UTCTime
  , downloadCompletedAt :: Maybe UTCTime
  , downloadImportedAt :: Maybe UTCTime
  , downloadUpdatedAt :: Maybe UTCTime
  , downloadMatchedClusterId :: Maybe Int64
  , downloadLibraryPath :: Maybe Text
  } deriving (Show, Eq)

-- | Quality profile record.
data QualityProfileRecord = QualityProfileRecord
  { qualityProfileId :: Maybe Int64
  , qualityProfileName :: Text
  , qualityProfileCutoffQuality :: Text
  , qualityProfileQualityPreferences :: Text  -- JSON
  , qualityProfileUpgradeAutomatically :: Bool
  , qualityProfileCreatedAt :: Maybe UTCTime
  , qualityProfileUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

-- | Helper function to convert PascalCase to snake_case.
-- Handles the first character specially to avoid leading underscore.
toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (c:cs) = toLower c : map toLower (go c cs)
  where
    go _ [] = []
    go prev (curr:rest)
      -- Insert underscore before uppercase letter that follows a lowercase letter
      | isLower prev && isUpper curr = '_' : curr : go curr rest
      -- Insert underscore before last uppercase in a sequence (e.g., "HTTPResponse" -> "http_response")
      | isUpper prev && isUpper curr = case rest of
          (nextChar:_) | isLower nextChar -> '_' : curr : go curr rest
          _ -> curr : go curr rest
      -- No underscore needed
      | otherwise = curr : go curr rest

-- | Convert SourceType to Text for database storage.
-- Uses Generic derivation with PascalCase → snake_case conversion.
sourceTypeToText :: SourceType -> Text
sourceTypeToText = toText . toSnakeCase . showConstr . toConstr

-- | Parse SourceType from Text (database representation).
textToSourceType :: Text -> Maybe SourceType
textToSourceType "library_artists" = Just LibraryArtists
textToSourceType "metacritic" = Just Metacritic
textToSourceType "pitchfork" = Just Pitchfork
-- Backward compatibility
textToSourceType "specific_artist" = Just LibraryArtists
textToSourceType "custom" = Just LibraryArtists
textToSourceType _ = Nothing

-- | Convert AlbumStatus to Text for database storage.
-- Uses Generic derivation with simple lowercase conversion.
albumStatusToText :: AlbumStatus -> Text
albumStatusToText = toText . map toLower . showConstr . toConstr

-- | Parse AlbumStatus from Text (database representation).
textToAlbumStatus :: Text -> Maybe AlbumStatus
textToAlbumStatus "wanted" = Just Wanted
textToAlbumStatus "upgrading" = Just Upgrading
textToAlbumStatus "acquired" = Just Acquired
textToAlbumStatus "ignored" = Just Ignored
textToAlbumStatus "monitoring" = Just Monitoring
textToAlbumStatus _ = Nothing

-- | Convert DownloadStatus to Text for database storage.
-- Uses Generic derivation, removes "Download" prefix and converts to snake_case.
downloadStatusToText :: DownloadStatus -> Text
downloadStatusToText = toText . toSnakeCase . drop 8 . showConstr . toConstr

-- | Parse DownloadStatus from Text (database representation).
textToDownloadStatus :: Text -> Maybe DownloadStatus
textToDownloadStatus "queued" = Just DownloadQueued
textToDownloadStatus "downloading" = Just DownloadDownloading
textToDownloadStatus "completed" = Just DownloadCompleted
textToDownloadStatus "failed" = Just DownloadFailed
textToDownloadStatus "imported" = Just DownloadImported
textToDownloadStatus "cancelled" = Just DownloadCancelled
textToDownloadStatus "identification_failure" = Just DownloadIdentificationFailure
textToDownloadStatus _ = Nothing

-- | Convert MatchSource to Text for database storage.
matchSourceToText :: MatchSource -> Text
matchSourceToText AutoFingerprint = "auto_fingerprint"
matchSourceToText AutoMetadata = "auto_metadata"
matchSourceToText Manual = "manual"

-- | Parse MatchSource from Text (database representation).
textToMatchSource :: Text -> Maybe MatchSource
textToMatchSource "auto_fingerprint" = Just AutoFingerprint
textToMatchSource "auto_metadata" = Just AutoMetadata
textToMatchSource "manual" = Just Manual
textToMatchSource _ = Nothing

-- FromRow instances (SQLite only)
instance SQLite.FromRow LibraryTrackRecord where
  fromRow = LibraryTrackRecord
    <$> SQLite.field
    <*> (toOsPath <$> SQLite.field)
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    where
      toOsPath :: String -> OsPath
      toOsPath = unsafePerformIO . OP.encodeUtf

instance SQLite.FromRow LibraryTrackMetadataRecord where
  fromRow = LibraryTrackMetadataRecord
    <$> SQLite.field  -- metaTrackId
    <*> SQLite.field  -- metaFormat
    <*> SQLite.field  -- metaTitle
    <*> SQLite.field  -- metaArtist
    <*> SQLite.field  -- metaAlbum
    <*> SQLite.field  -- metaAlbumArtist
    <*> SQLite.field  -- metaTrackNumber
    <*> SQLite.field  -- metaTotalTracks
    <*> SQLite.field  -- metaDiscNumber
    <*> SQLite.field  -- metaTotalDiscs
    <*> SQLite.field  -- metaDate
    <*> SQLite.field  -- metaYear
    <*> SQLite.field  -- metaGenre
    <*> SQLite.field  -- metaPublisher
    <*> SQLite.field  -- metaComment
    <*> SQLite.field  -- metaDurationSeconds
    <*> SQLite.field  -- metaBitsPerSample
    <*> SQLite.field  -- metaReleaseCountry
    <*> SQLite.field  -- metaRecordLabel
    <*> SQLite.field  -- metaCatalogNumber
    <*> SQLite.field  -- metaBarcode
    <*> SQLite.field  -- metaReleaseStatus
    <*> SQLite.field  -- metaReleaseType
    <*> SQLite.field  -- metaMBRecordingId
    <*> SQLite.field  -- metaMBTrackId
    <*> SQLite.field  -- metaMBReleaseId
    <*> SQLite.field  -- metaMBReleaseGroupId
    <*> SQLite.field  -- metaMBArtistId
    <*> SQLite.field  -- metaMBAlbumArtistId
    <*> SQLite.field  -- metaMBWorkId
    <*> SQLite.field  -- metaMBDiscId
    <*> SQLite.field  -- metaAcoustidFingerprint
    <*> SQLite.field  -- metaAcoustidId
    <*> SQLite.field  -- metaCreatedAt
    <*> SQLite.field  -- metaUpdatedAt

instance SQLite.FromRow ScanHistoryRecord where
  fromRow = ScanHistoryRecord
    <$> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> SQLite.field
    <*> (toBool <$> SQLite.field)
    <*> SQLite.field
    where
      toBool :: Int -> Bool
      toBool 0 = False
      toBool _ = True

instance SQLite.FromRow MetadataDiffRecord where
  fromRow = MetadataDiffRecord
    <$> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> SQLite.field

instance SQLite.FromRow MetadataChangeRecord where
  fromRow = MetadataChangeRecord
    <$> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field

instance SQLite.FromRow ClusterRecord where
  fromRow = do
    cId <- SQLite.field
    metaHash <- SQLite.field
    album <- SQLite.field
    albumArtist <- SQLite.field
    trackCnt <- SQLite.field
    mbRelId <- SQLite.field
    mbRelGrpId <- SQLite.field
    mbConf <- SQLite.field
    createdAt <- SQLite.field
    updatedAt <- SQLite.field
    lastIdAt <- SQLite.field
    mbRelData <- SQLite.field
    mbCands <- SQLite.field
    matchSrcText <- SQLite.field :: SQLite.RowParser (Maybe Text)
    matchLck <- toBool <$> SQLite.field
    let matchSrc = matchSrcText >>= textToMatchSource
    pure $ ClusterRecord cId metaHash album albumArtist trackCnt mbRelId mbRelGrpId mbConf createdAt updatedAt lastIdAt mbRelData mbCands matchSrc matchLck
    where
      toBool :: Int -> Bool
      toBool 0 = False
      toBool _ = True

instance SQLite.FromRow AcquisitionSourceRecord where
  fromRow = do
    sId <- SQLite.field
    name <- SQLite.field
    desc <- SQLite.field
    sTypeText <- SQLite.field :: SQLite.RowParser Text
    _ <- SQLite.field :: SQLite.RowParser (Maybe Text)  -- Discard deprecated artist_mbid field
    enabled <- toBool <$> SQLite.field
    _ <- SQLite.field :: SQLite.RowParser Int  -- Discard deprecated priority field
    filters <- SQLite.field
    createdAt <- SQLite.field
    updatedAt <- SQLite.field
    let sType = fromMaybe LibraryArtists (textToSourceType sTypeText)
    pure $ AcquisitionSourceRecord sId name desc sType enabled filters createdAt updatedAt
    where
      toBool :: Int -> Bool
      toBool 0 = False
      toBool _ = True

instance SQLite.FromRow TrackedArtistRecord where
  fromRow = TrackedArtistRecord
    <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field

instance SQLite.FromRow WantedAlbumRecord where
  fromRow = do
    wId <- SQLite.field
    rgMBID <- SQLite.field
    title <- SQLite.field
    artistMBID <- SQLite.field
    artistName <- SQLite.field
    statusText <- SQLite.field :: SQLite.RowParser Text
    addedBySourceId <- SQLite.field
    releaseDate <- SQLite.field
    matchedClusterId <- SQLite.field
    createdAt <- SQLite.field
    updatedAt <- SQLite.field
    let status = fromMaybe Wanted (textToAlbumStatus statusText)
    pure $ WantedAlbumRecord wId rgMBID title artistMBID artistName status addedBySourceId releaseDate matchedClusterId createdAt updatedAt

instance SQLite.FromRow CatalogArtistRecord where
  fromRow = CatalogArtistRecord
    <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> (toBool <$> SQLite.field)
    <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field
    where
      toBool :: Int -> Bool
      toBool 0 = False
      toBool _ = True

instance SQLite.FromRow CatalogAlbumRecord where
  fromRow = CatalogAlbumRecord
    <$> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> SQLite.field <*> SQLite.field
    <*> SQLite.field <*> SQLite.field <*> SQLite.field

instance SQLite.FromRow DownloadRecord where
  fromRow = do
    dId <- SQLite.field
    dlCatalogAlbumId <- SQLite.field
    indexerName <- SQLite.field
    url <- SQLite.field
    client <- SQLite.field
    clientId <- SQLite.field
    statusText <- SQLite.field :: SQLite.RowParser Text
    path <- SQLite.field
    title <- SQLite.field
    sizeBytes <- SQLite.field
    quality <- SQLite.field
    format <- SQLite.field
    seeders <- SQLite.field
    progress <- SQLite.field
    errorMsg <- SQLite.field
    queuedAt <- SQLite.field
    startedAt <- SQLite.field
    completedAt <- SQLite.field
    importedAt <- SQLite.field
    updatedAt <- SQLite.field
    matchedClusterId <- SQLite.field
    libraryPath <- SQLite.field
    let status = fromMaybe DownloadQueued (textToDownloadStatus statusText)
    pure $ DownloadRecord dId dlCatalogAlbumId indexerName url client clientId status path title sizeBytes quality format seeders progress errorMsg queuedAt startedAt completedAt importedAt updatedAt matchedClusterId libraryPath

instance SQLite.FromRow QualityProfileRecord where
  fromRow = do
    qpId <- SQLite.field
    name <- SQLite.field
    cutoffQuality <- SQLite.field
    qualityPrefs <- SQLite.field
    upgradeAuto <- toBool <$> SQLite.field
    createdAt <- SQLite.field
    updatedAt <- SQLite.field
    pure $ QualityProfileRecord qpId name cutoffQuality qualityPrefs upgradeAuto createdAt updatedAt
    where
      toBool :: Int -> Bool
      toBool 0 = False
      toBool _ = True
