{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Domain event types for the event-driven architecture.
module Skema.Events.Types
  ( Event(..)
  , EventMetadata(..)
  , EventEnvelope(..)
  , eventType
  , eventToJSON
  ) where

import GHC.Generics ()
import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Data (Data, toConstr, showConstr)

-- | Core domain events that drive the system.
data Event
  -- Library scan events
  = LibraryScanRequested
      { scanPath :: Text
      , forceRescan :: Bool  -- If True, ignore previous snapshot and treat as initial scan
      }
  | FileSystemDiffGenerated
      { filesAdded :: Int
      , filesModified :: Int
      , filesDeleted :: Int
      }
  | MetadataReadStarted
      { filesToRead :: Int
      }
  | MetadataReadProgress
      { currentFile :: Text
      , filesProcessed :: Int
      , filesToRead :: Int
      }
  | MetadataReadComplete
      { filesProcessed :: Int
      , readErrors :: Int
      }
  | ClustersGenerated
      { totalGroups :: Int
      , alreadyMatched :: Int
      , needsIdentification :: Int
      }
  | ResultsPersisted
      { filesUpdated :: Int
      }

  -- Identification events
  | IdentificationStarted
      { groupCount :: Int
      }
  | IdentificationProgress
      { currentAlbum :: Text
      , currentArtist :: Text
      , albumsProcessed :: Int
      , totalAlbums :: Int
      }
  | ClusterIdentified
      { identifiedClusterId :: Int64
      , identifiedReleaseId :: Text
      , identifiedReleaseGroupId :: Maybe Text
      , identifiedConfidence :: Double
      , identifiedTrackCount :: Int
      }
  | IdentificationComplete
      { groupsProcessed :: Int
      , matchesFound :: Int
      }

  -- Acquisition events
  | LibraryArtistFound
      { foundArtistMBID :: Text
      , foundArtistName :: Text
      , foundClusterId :: Int64
      , foundReleaseGroupId :: Maybe Text
      }
  | TrackedArtistAdded
      { trackedArtistId :: Int64
      , trackedArtistMBID :: Text
      , trackedArtistName :: Text
      , trackedArtistClusterId :: Int64
      , trackedArtistReleaseGroupId :: Maybe Text
      }
  | CatalogArtistFollowed
      { catalogArtistMBID :: Text
      , catalogArtistName :: Text
      }
  | CatalogArtistRefreshRequested
      { refreshArtistMBID :: Text
      }
  | ArtistDiscographyRequested
      { requestedArtistId :: Int64
      , requestedArtistMBID :: Text
      }
  | CatalogAlbumAdded
      { catalogAlbumId :: Int64
      , catalogAlbumReleaseGroupMBID :: Text
      , catalogAlbumTitle :: Text
      , catalogAlbumArtistId :: Int64
      , catalogAlbumArtistMBID :: Text
      , catalogAlbumArtistName :: Text
      , catalogAlbumType :: Maybe Text
      , catalogAlbumFirstReleaseDate :: Maybe Text
      , catalogAlbumWanted :: Bool
      }
  | ArtistImageFetched
      { artistImageId :: Int64
      , artistImageMBID :: Text
      , artistImageUrl :: Text
      , artistImageThumbnailUrl :: Maybe Text
      , artistImageSource :: Text  -- e.g., "MusicBrainz", "Last.fm"
      }
  | ArtistDiscographyFetched
      { artistDiscographyArtistId :: Int64
      , artistMBID :: Text
      , artistDiscographyArtistName :: Text
      , releaseGroupCount :: Int
      , artistLastCheckedAt :: Text
      }
  | WantedAlbumAdded
      { wantedCatalogAlbumId :: Int64
      , wantedReleaseGroupId :: Text
      , wantedAlbumTitle :: Text
      , wantedArtistName :: Text
      }
  | AlbumCoverFetched
      { albumCoverReleaseGroupMBID :: Text
      , albumCoverUrl :: Text
      , albumCoverThumbnailUrl :: Maybe Text
      , albumCoverSource :: Text
      }

  -- Search/Orchestration events
  | AlbumSearchStarted
      { searchAlbumTitle :: Text
      , searchArtistName :: Text
      , searchIndexerCount :: Int
      }
  | IndexerSearchCompleted
      { searchIndexerName :: Text
      , searchResultCount :: Int
      , searchDuration :: Double
      }
  | AlbumSearchCompleted
      { searchTotalResults :: Int
      , searchBestScore :: Maybe Int
      , searchDuration :: Double
      }
  | BestReleaseSelected
      { selectedTitle :: Text
      , selectedIndexer :: Text
      , selectedScore :: Int
      , selectedSeeders :: Maybe Int
      }

  -- Download events
  | DownloadQueued
      { downloadId :: Int64
      , downloadTitle :: Text
      , downloadClient :: Text
      }
  | DownloadStarted
      { downloadId :: Int64
      , downloadTitle :: Text
      }
  | DownloadProgress
      { downloadId :: Int64
      , downloadTitle :: Text
      , downloadProgress :: Double
      , downloadSizeBytes :: Maybe Integer
      , downloadedBytes :: Maybe Integer
      }
  | DownloadCompleted
      { downloadId :: Int64
      , downloadTitle :: Text
      , downloadPath :: Maybe Text
      }
  | DownloadFailed
      { downloadId :: Int64
      , downloadTitle :: Text
      , downloadError :: Maybe Text
      }
  | DownloadImported
      { downloadId :: Int64
      , downloadTitle :: Text
      }

  -- Metadata diff events
  | MetadataDiffGenerated
      { diffFileId :: Int64
      , diffFieldName :: Text
      , diffFileValue :: Maybe Text
      , diffMBValue :: Maybe Text
      }
  | TrackDiffsGenerated
      { diffTrackId :: Int64
      , diffClusterId :: Int64
      , diffCount :: Int
      }
  | TracksRematched
      { rematchedClusterId :: Int64
      , rematchedTrackCount :: Int
      }
  | MetadataDiffApplied
      { diffId :: Int64
      }
  | GroupedDiffsApplied
      { fieldName :: Text
      , affectedFileCount :: Int
      }
  | MetadataWriteRequested
      { writeDiffIds :: [Int64]
      }
  | MetadataWriteStarted
      { writeTotalChanges :: Int
      }
  | MetadataWriteProgress
      { writeCurrentFile :: Text
      , writeChangesProcessed :: Int
      , writeTotalChanges :: Int
      }
  | MetadataWriteCompleted
      { writeChangesApplied :: Int
      , writeErrors :: Int
      }
  | MetadataWriteFailed
      { writeErrorMessage :: Text
      }

  -- Stats events
  | StatsUpdated
      { totalFiles :: Int
      , totalAlbums :: Int
      , totalArtists :: Int
      , matchedFiles :: Int
      , unmatchedFiles :: Int
      , metadataAccuracy :: Double
      , totalDiffs :: Int
      , librarySize :: Int
      , totalRuntime :: Int
      }

  -- Quality events
  | AlbumQualityDetected
      { qualityAlbumId :: Int64
      , qualityDetected :: Text  -- Quality name (e.g., "lossless", "mp3_320")
      , qualityPrevious :: Maybe Text  -- Previous quality if any
      }
  | AlbumUpgradeNeeded
      { upgradeAlbumId :: Int64
      , upgradeCurrentQuality :: Text
      , upgradeTargetQuality :: Text  -- Cutoff quality
      , upgradeProfileName :: Text
      }
  | AlbumQualityMet
      { metAlbumId :: Int64
      , metQuality :: Text
      , metProfileName :: Text
      }
  | QualityProfileCreated
      { profileCreatedId :: Int64
      , profileCreatedName :: Text
      }
  | QualityProfileUpdated
      { profileUpdatedId :: Int64
      , profileUpdatedName :: Text
      }
  | QualityProfileDeleted
      { profileDeletedId :: Int64
      , profileDeletedName :: Text
      }

  -- System events
  | ConfigUpdated
      { updatedConfig :: Value  -- JSON value containing the updated config
      }
  | ConfigReloadFailed
      { configReloadError :: Text
      }
  | Heartbeat

  -- Task events
  | TaskCreated
      { taskCreatedId :: Text
      , taskCreatedResource :: Text
      , taskCreatedType :: Text
      }
  | TaskProgressUpdated
      { taskProgressId :: Text
      , taskProgressValue :: Double
      , taskProgressMessage :: Maybe Text
      }
  | TaskCompleted
      { taskCompletedId :: Text
      , taskCompletedResult :: Maybe Value
      }
  | TaskFailed'
      { taskFailedId :: Text
      , taskFailedError :: Text
      }
  | TaskCancelled'
      { taskCancelledId :: Text
      }
  deriving (Show, Eq, Generic, Data)

-- | Metadata attached to every event.
data EventMetadata = EventMetadata
  { eventId :: UUID
  , eventTimestamp :: UTCTime
  , eventSource :: Text  -- e.g., "api", "task-runner", "scanner"
  , eventSequence :: Int64  -- Monotonically increasing sequence number for ordering
  } deriving (Show, Eq, Generic)

instance ToJSON EventMetadata where
  toJSON (EventMetadata eid ts src seqNum) = object
    [ "id" .= (show eid :: Text)
    , "timestamp" .= ts
    , "source" .= src
    , "sequence" .= seqNum
    ]

-- | Event wrapped with metadata for transport.
data EventEnvelope = EventEnvelope
  { envelopeMetadata :: EventMetadata
  , envelopeEvent :: Event
  } deriving (Show, Eq, Generic)

instance ToJSON EventEnvelope where
  toJSON (EventEnvelope meta evt) = object
    [ "metadata" .= meta
    , "type" .= eventType evt
    , "data" .= eventToJSON evt
    ]

-- | Get the event type name as a string (using PascalCase constructor names).
-- Uses Generic derivation to extract the constructor name automatically.
eventType :: Event -> Text
eventType = toText . showConstr . toConstr

-- | Convert event data to JSON.
eventToJSON :: Event -> Value
eventToJSON = \case
  LibraryScanRequested{..} -> object
    [ "scan_path" .= scanPath
    , "force_rescan" .= forceRescan
    ]
  FileSystemDiffGenerated{..} -> object
    [ "files_added" .= filesAdded
    , "files_modified" .= filesModified
    , "files_deleted" .= filesDeleted
    ]
  MetadataReadStarted{..} -> object
    [ "files_to_read" .= filesToRead
    ]
  MetadataReadProgress{..} -> object
    [ "current_file" .= currentFile
    , "files_processed" .= filesProcessed
    , "files_to_read" .= filesToRead
    ]
  MetadataReadComplete{..} -> object
    [ "files_processed" .= filesProcessed
    , "read_errors" .= readErrors
    ]
  ClustersGenerated{..} -> object
    [ "total_groups" .= totalGroups
    , "already_matched" .= alreadyMatched
    , "needs_identification" .= needsIdentification
    ]
  ResultsPersisted{..} -> object
    [ "files_updated" .= filesUpdated
    ]
  IdentificationStarted{..} -> object
    [ "group_count" .= groupCount
    ]
  IdentificationProgress{..} -> object
    [ "current_album" .= currentAlbum
    , "current_artist" .= currentArtist
    , "albums_processed" .= albumsProcessed
    , "total_albums" .= totalAlbums
    ]
  ClusterIdentified{..} -> object
    [ "cluster_id" .= identifiedClusterId
    , "release_id" .= identifiedReleaseId
    , "release_group_id" .= identifiedReleaseGroupId
    , "confidence" .= identifiedConfidence
    , "track_count" .= identifiedTrackCount
    ]
  IdentificationComplete{..} -> object
    [ "groups_processed" .= groupsProcessed
    , "matches_found" .= matchesFound
    ]
  LibraryArtistFound{..} -> object
    [ "artist_mbid" .= foundArtistMBID
    , "artist_name" .= foundArtistName
    , "cluster_id" .= foundClusterId
    , "release_group_id" .= foundReleaseGroupId
    ]
  TrackedArtistAdded{..} -> object
    [ "artist_id" .= trackedArtistId
    , "artist_mbid" .= trackedArtistMBID
    , "artist_name" .= trackedArtistName
    , "cluster_id" .= trackedArtistClusterId
    , "release_group_id" .= trackedArtistReleaseGroupId
    ]
  CatalogArtistFollowed{..} -> object
    [ "artist_mbid" .= catalogArtistMBID
    , "artist_name" .= catalogArtistName
    ]
  CatalogArtistRefreshRequested{..} -> object
    [ "artist_mbid" .= refreshArtistMBID
    ]
  ArtistDiscographyRequested{..} -> object
    [ "artist_id" .= requestedArtistId
    , "artist_mbid" .= requestedArtistMBID
    ]
  CatalogAlbumAdded{..} -> object
    [ "album_id" .= catalogAlbumId
    , "release_group_mbid" .= catalogAlbumReleaseGroupMBID
    , "album_title" .= catalogAlbumTitle
    , "artist_id" .= catalogAlbumArtistId
    , "artist_mbid" .= catalogAlbumArtistMBID
    , "artist_name" .= catalogAlbumArtistName
    , "album_type" .= catalogAlbumType
    , "first_release_date" .= catalogAlbumFirstReleaseDate
    , "wanted" .= catalogAlbumWanted
    ]
  ArtistImageFetched{..} -> object
    [ "artist_id" .= artistImageId
    , "artist_mbid" .= artistImageMBID
    , "image_url" .= artistImageUrl
    , "thumbnail_url" .= artistImageThumbnailUrl
    , "source" .= artistImageSource
    ]
  ArtistDiscographyFetched{..} -> object
    [ "artist_id" .= artistDiscographyArtistId
    , "artist_mbid" .= artistMBID
    , "artist_name" .= artistDiscographyArtistName
    , "release_group_count" .= releaseGroupCount
    , "last_checked_at" .= artistLastCheckedAt
    ]
  WantedAlbumAdded{..} -> object
    [ "catalog_album_id" .= wantedCatalogAlbumId
    , "release_group_id" .= wantedReleaseGroupId
    , "album_title" .= wantedAlbumTitle
    , "artist_name" .= wantedArtistName
    ]
  AlbumCoverFetched{..} -> object
    [ "release_group_mbid" .= albumCoverReleaseGroupMBID
    , "cover_url" .= albumCoverUrl
    , "thumbnail_url" .= albumCoverThumbnailUrl
    , "source" .= albumCoverSource
    ]
  AlbumSearchStarted{..} -> object
    [ "album_title" .= searchAlbumTitle
    , "artist_name" .= searchArtistName
    , "indexer_count" .= searchIndexerCount
    ]
  IndexerSearchCompleted{..} -> object
    [ "indexer_name" .= searchIndexerName
    , "result_count" .= searchResultCount
    , "duration" .= searchDuration
    ]
  AlbumSearchCompleted{..} -> object
    [ "total_results" .= searchTotalResults
    , "best_score" .= searchBestScore
    , "duration" .= searchDuration
    ]
  BestReleaseSelected{..} -> object
    [ "title" .= selectedTitle
    , "indexer" .= selectedIndexer
    , "score" .= selectedScore
    , "seeders" .= selectedSeeders
    ]
  DownloadQueued{..} -> object
    [ "download_id" .= downloadId
    , "download_title" .= downloadTitle
    , "download_client" .= downloadClient
    ]
  DownloadStarted{..} -> object
    [ "download_id" .= downloadId
    , "download_title" .= downloadTitle
    ]
  DownloadProgress{..} -> object
    [ "download_id" .= downloadId
    , "download_title" .= downloadTitle
    , "download_progress" .= downloadProgress
    , "download_size_bytes" .= downloadSizeBytes
    , "downloaded_bytes" .= downloadedBytes
    ]
  DownloadCompleted{..} -> object
    [ "download_id" .= downloadId
    , "download_title" .= downloadTitle
    , "download_path" .= downloadPath
    ]
  DownloadFailed{..} -> object
    [ "download_id" .= downloadId
    , "download_title" .= downloadTitle
    , "download_error" .= downloadError
    ]
  DownloadImported{..} -> object
    [ "download_id" .= downloadId
    , "download_title" .= downloadTitle
    ]
  MetadataDiffGenerated{..} -> object
    [ "file_id" .= diffFileId
    , "field_name" .= diffFieldName
    , "file_value" .= diffFileValue
    , "mb_value" .= diffMBValue
    ]
  TrackDiffsGenerated{..} -> object
    [ "track_id" .= diffTrackId
    , "cluster_id" .= diffClusterId
    , "diff_count" .= diffCount
    ]
  TracksRematched{..} -> object
    [ "cluster_id" .= rematchedClusterId
    , "track_count" .= rematchedTrackCount
    ]
  MetadataDiffApplied{..} -> object
    [ "diff_id" .= diffId
    ]
  GroupedDiffsApplied{..} -> object
    [ "field_name" .= fieldName
    , "affected_file_count" .= affectedFileCount
    ]
  MetadataWriteRequested{..} -> object
    [ "diff_ids" .= writeDiffIds
    ]
  MetadataWriteStarted{..} -> object
    [ "total_changes" .= writeTotalChanges
    ]
  MetadataWriteProgress{..} -> object
    [ "current_file" .= writeCurrentFile
    , "changes_processed" .= writeChangesProcessed
    , "total_changes" .= writeTotalChanges
    ]
  MetadataWriteCompleted{..} -> object
    [ "changes_applied" .= writeChangesApplied
    , "errors" .= writeErrors
    ]
  MetadataWriteFailed{..} -> object
    [ "error_message" .= writeErrorMessage
    ]
  StatsUpdated{..} -> object
    [ "total_files" .= totalFiles
    , "total_albums" .= totalAlbums
    , "total_artists" .= totalArtists
    , "matched_files" .= matchedFiles
    , "unmatched_files" .= unmatchedFiles
    , "metadata_accuracy" .= metadataAccuracy
    , "total_diffs" .= totalDiffs
    , "library_size" .= librarySize
    , "total_runtime" .= totalRuntime
    ]
  AlbumQualityDetected{..} -> object
    [ "album_id" .= qualityAlbumId
    , "quality_detected" .= qualityDetected
    , "quality_previous" .= qualityPrevious
    ]
  AlbumUpgradeNeeded{..} -> object
    [ "album_id" .= upgradeAlbumId
    , "current_quality" .= upgradeCurrentQuality
    , "target_quality" .= upgradeTargetQuality
    , "profile_name" .= upgradeProfileName
    ]
  AlbumQualityMet{..} -> object
    [ "album_id" .= metAlbumId
    , "quality" .= metQuality
    , "profile_name" .= metProfileName
    ]
  QualityProfileCreated{..} -> object
    [ "profile_id" .= profileCreatedId
    , "profile_name" .= profileCreatedName
    ]
  QualityProfileUpdated{..} -> object
    [ "profile_id" .= profileUpdatedId
    , "profile_name" .= profileUpdatedName
    ]
  QualityProfileDeleted{..} -> object
    [ "profile_id" .= profileDeletedId
    , "profile_name" .= profileDeletedName
    ]
  ConfigUpdated{..} -> updatedConfig  -- Return the config value directly
  ConfigReloadFailed{..} -> object
    [ "error" .= configReloadError
    ]
  Heartbeat -> object []
  TaskCreated{..} -> object
    [ "task_id" .= taskCreatedId
    , "resource" .= taskCreatedResource
    , "type" .= taskCreatedType
    ]
  TaskProgressUpdated{..} -> object
    [ "task_id" .= taskProgressId
    , "progress" .= taskProgressValue
    , "message" .= taskProgressMessage
    ]
  TaskCompleted{..} -> object
    [ "task_id" .= taskCompletedId
    , "result" .= taskCompletedResult
    ]
  TaskFailed'{..} -> object
    [ "task_id" .= taskFailedId
    , "error" .= taskFailedError
    ]
  TaskCancelled'{..} -> object
    [ "task_id" .= taskCancelledId
    ]
