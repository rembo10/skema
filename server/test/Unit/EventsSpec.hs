{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests pinning the wire format of every domain event.
--
-- Each 'Event' constructor is serialized once and compared against its
-- expected JSON payload, so renaming a field or key shows up as a test
-- failure instead of silently breaking SSE consumers. A completeness
-- check (via 'Data') fails when a constructor is added without a golden
-- case here.
module Unit.EventsSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (Value (..), object, (.=), toJSON)
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr)
import Data.Time (UTCTime (..), fromGregorian)
import qualified Data.UUID as UUID

import Skema.Events.Types

tests :: TestTree
tests = testGroup "Unit.Events"
  [ goldenPayloadTests
  , constructorCompletenessTest
  , envelopeTest
  ]

-- | One entry per Event constructor: the event, its expected @type@
-- string, and its expected @data@ payload.
goldenCases :: [(Text, Event, Value)]
goldenCases =
  [ ( "LibraryScanRequested"
    , LibraryScanRequested { scanPath = "/music", forceRescan = True }
    , object
        [ "scan_path" .= ("/music" :: Text)
        , "force_rescan" .= True
        ]
    )
  , ( "FileSystemDiffGenerated"
    , FileSystemDiffGenerated { filesAdded = 1, filesModified = 2, filesDeleted = 3 }
    , object
        [ "files_added" .= (1 :: Int)
        , "files_modified" .= (2 :: Int)
        , "files_deleted" .= (3 :: Int)
        ]
    )
  , ( "MetadataReadStarted"
    , MetadataReadStarted { filesToRead = 10 }
    , object [ "files_to_read" .= (10 :: Int) ]
    )
  , ( "MetadataReadProgress"
    , MetadataReadProgress { currentFile = "a.flac", filesProcessed = 4, filesToRead = 10 }
    , object
        [ "current_file" .= ("a.flac" :: Text)
        , "files_processed" .= (4 :: Int)
        , "files_to_read" .= (10 :: Int)
        ]
    )
  , ( "MetadataReadComplete"
    , MetadataReadComplete { filesProcessed = 10, readErrors = 2 }
    , object
        [ "files_processed" .= (10 :: Int)
        , "read_errors" .= (2 :: Int)
        ]
    )
  , ( "ClustersGenerated"
    , ClustersGenerated
        { totalGroups = 5
        , alreadyMatched = 3
        , needsIdentification = 2
        , affectedClusterIds = [11, 12]
        }
    , object
        [ "total_groups" .= (5 :: Int)
        , "already_matched" .= (3 :: Int)
        , "needs_identification" .= (2 :: Int)
        , "affected_cluster_ids" .= ([11, 12] :: [Int64])
        ]
    )
  , ( "IdentificationStarted"
    , IdentificationStarted { groupCount = 6 }
    , object [ "group_count" .= (6 :: Int) ]
    )
  , ( "IdentificationProgress"
    , IdentificationProgress
        { currentAlbum = "OK Computer"
        , currentArtist = "Radiohead"
        , albumsProcessed = 2
        , totalAlbums = 9
        }
    , object
        [ "current_album" .= ("OK Computer" :: Text)
        , "current_artist" .= ("Radiohead" :: Text)
        , "albums_processed" .= (2 :: Int)
        , "total_albums" .= (9 :: Int)
        ]
    )
  , ( "ClusterIdentified"
    , ClusterIdentified
        { identifiedClusterId = 7
        , identifiedReleaseId = "rel-1"
        , identifiedReleaseGroupId = Just "rg-1"
        , identifiedConfidence = 0.95
        , identifiedTrackCount = 12
        }
    , object
        [ "cluster_id" .= (7 :: Int64)
        , "release_id" .= ("rel-1" :: Text)
        , "release_group_id" .= ("rg-1" :: Text)
        , "confidence" .= (0.95 :: Double)
        , "track_count" .= (12 :: Int)
        ]
    )
  , ( "IdentificationComplete"
    , IdentificationComplete { groupsProcessed = 9, matchesFound = 8 }
    , object
        [ "groups_processed" .= (9 :: Int)
        , "matches_found" .= (8 :: Int)
        ]
    )
  , ( "LibraryArtistFound"
    , LibraryArtistFound
        { foundArtistMBID = "mbid-a"
        , foundArtistName = "Artist A"
        , foundClusterId = 21
        , foundReleaseGroupId = Just "rg-2"
        }
    , object
        [ "artist_mbid" .= ("mbid-a" :: Text)
        , "artist_name" .= ("Artist A" :: Text)
        , "cluster_id" .= (21 :: Int64)
        , "release_group_id" .= ("rg-2" :: Text)
        ]
    )
  , ( "TrackedArtistAdded"
    , TrackedArtistAdded
        { trackedArtistId = 31
        , trackedArtistMBID = "mbid-b"
        , trackedArtistName = "Artist B"
        , trackedArtistClusterId = 22
        , trackedArtistReleaseGroupId = Nothing
        }
    , object
        [ "artist_id" .= (31 :: Int64)
        , "artist_mbid" .= ("mbid-b" :: Text)
        , "artist_name" .= ("Artist B" :: Text)
        , "cluster_id" .= (22 :: Int64)
        , "release_group_id" .= Null
        ]
    )
  , ( "CatalogArtistFollowed"
    , CatalogArtistFollowed { catalogArtistMBID = "mbid-c", catalogArtistName = "Artist C" }
    , object
        [ "artist_mbid" .= ("mbid-c" :: Text)
        , "artist_name" .= ("Artist C" :: Text)
        ]
    )
  , ( "CatalogArtistRefreshRequested"
    , CatalogArtistRefreshRequested { refreshArtistMBID = "mbid-d" }
    , object [ "artist_mbid" .= ("mbid-d" :: Text) ]
    )
  , ( "ArtistDiscographyRequested"
    , ArtistDiscographyRequested { requestedArtistId = 41, requestedArtistMBID = "mbid-e" }
    , object
        [ "artist_id" .= (41 :: Int64)
        , "artist_mbid" .= ("mbid-e" :: Text)
        ]
    )
  , ( "CatalogAlbumAdded"
    , CatalogAlbumAdded
        { catalogAlbumId = 51
        , catalogAlbumReleaseGroupMBID = "rg-3"
        , catalogAlbumTitle = "Album X"
        , catalogAlbumArtistId = 42
        , catalogAlbumArtistMBID = "mbid-f"
        , catalogAlbumArtistName = "Artist F"
        , catalogAlbumType = Just "Album"
        , catalogAlbumFirstReleaseDate = Just "2001-01-01"
        , catalogAlbumWanted = True
        }
    , object
        [ "album_id" .= (51 :: Int64)
        , "release_group_mbid" .= ("rg-3" :: Text)
        , "album_title" .= ("Album X" :: Text)
        , "artist_id" .= (42 :: Int64)
        , "artist_mbid" .= ("mbid-f" :: Text)
        , "artist_name" .= ("Artist F" :: Text)
        , "album_type" .= ("Album" :: Text)
        , "first_release_date" .= ("2001-01-01" :: Text)
        , "wanted" .= True
        ]
    )
  , ( "CatalogAlbumUpdated"
    , CatalogAlbumUpdated
        { updatedAlbumId = 52
        , updatedAlbumReleaseGroupMBID = "rg-4"
        , updatedAlbumTitle = "Album Y"
        , updatedAlbumArtistMBID = "mbid-g"
        , updatedAlbumArtistName = "Artist G"
        , updatedAlbumType = Just "EP"
        , updatedAlbumFirstReleaseDate = Just "2002-02-02"
        , updatedAlbumState = "wanted"
        , updatedAlbumCurrentQuality = Just "mp3_320"
        , updatedAlbumQualityProfileId = Just 61
        , updatedAlbumQualityProfileName = Just "Default"
        , updatedAlbumCoverUrl = Just "http://covers/1"
        , updatedAlbumCoverThumbnailUrl = Just "http://thumbs/1"
        }
    , object
        [ "album_id" .= (52 :: Int64)
        , "release_group_mbid" .= ("rg-4" :: Text)
        , "album_title" .= ("Album Y" :: Text)
        , "artist_mbid" .= ("mbid-g" :: Text)
        , "artist_name" .= ("Artist G" :: Text)
        , "album_type" .= ("EP" :: Text)
        , "first_release_date" .= ("2002-02-02" :: Text)
        , "state" .= ("wanted" :: Text)
        , "current_quality" .= ("mp3_320" :: Text)
        , "quality_profile_id" .= (61 :: Int64)
        , "quality_profile_name" .= ("Default" :: Text)
        , "cover_url" .= ("http://covers/1" :: Text)
        , "cover_thumbnail_url" .= ("http://thumbs/1" :: Text)
        ]
    )
  , ( "ArtistImageFetched"
    , ArtistImageFetched
        { artistImageId = 71
        , artistImageMBID = "mbid-h"
        , artistImageUrl = "http://images/1"
        , artistImageThumbnailUrl = Just "http://thumbs/2"
        , artistImageSource = "Last.fm"
        }
    , object
        [ "artist_id" .= (71 :: Int64)
        , "artist_mbid" .= ("mbid-h" :: Text)
        , "image_url" .= ("http://images/1" :: Text)
        , "thumbnail_url" .= ("http://thumbs/2" :: Text)
        , "source" .= ("Last.fm" :: Text)
        ]
    )
  , ( "ArtistBioFetched"
    , ArtistBioFetched { artistBioId = 72, artistBioText = "A bio" }
    , object
        [ "artist_id" .= (72 :: Int64)
        , "bio" .= ("A bio" :: Text)
        ]
    )
  , ( "ArtistDiscographyFetched"
    , ArtistDiscographyFetched
        { artistDiscographyArtistId = 73
        , artistMBID = "mbid-i"
        , artistDiscographyArtistName = "Artist I"
        , releaseGroupCount = 14
        , artistLastCheckedAt = "2026-07-25T00:00:00Z"
        }
    , object
        [ "artist_id" .= (73 :: Int64)
        , "artist_mbid" .= ("mbid-i" :: Text)
        , "artist_name" .= ("Artist I" :: Text)
        , "release_group_count" .= (14 :: Int)
        , "last_checked_at" .= ("2026-07-25T00:00:00Z" :: Text)
        ]
    )
  , ( "WantedAlbumAdded"
    , WantedAlbumAdded
        { wantedCatalogAlbumId = 81
        , wantedReleaseGroupId = "rg-5"
        , wantedAlbumTitle = "Album Z"
        , wantedArtistName = "Artist Z"
        }
    , object
        [ "catalog_album_id" .= (81 :: Int64)
        , "release_group_id" .= ("rg-5" :: Text)
        , "album_title" .= ("Album Z" :: Text)
        , "artist_name" .= ("Artist Z" :: Text)
        ]
    )
  , ( "AlbumCoverFetched"
    , AlbumCoverFetched
        { albumCoverReleaseGroupMBID = "rg-6"
        , albumCoverUrl = "http://covers/2"
        , albumCoverThumbnailUrl = Nothing
        , albumCoverSource = "CoverArtArchive"
        }
    , object
        [ "release_group_mbid" .= ("rg-6" :: Text)
        , "cover_url" .= ("http://covers/2" :: Text)
        , "thumbnail_url" .= Null
        , "source" .= ("CoverArtArchive" :: Text)
        ]
    )
  , ( "ArtistImageMissing"
    , ArtistImageMissing { missingArtistMBID = "mbid-j" }
    , object [ "artist_mbid" .= ("mbid-j" :: Text) ]
    )
  , ( "AlbumCoverMissing"
    , AlbumCoverMissing { missingReleaseGroupMBID = "rg-7" }
    , object [ "release_group_mbid" .= ("rg-7" :: Text) ]
    )
  , ( "AlbumSearchStarted"
    , AlbumSearchStarted
        { searchAlbumTitle = "Album W"
        , searchArtistName = "Artist W"
        , searchIndexerCount = 3
        }
    , object
        [ "album_title" .= ("Album W" :: Text)
        , "artist_name" .= ("Artist W" :: Text)
        , "indexer_count" .= (3 :: Int)
        ]
    )
  , ( "IndexerSearchCompleted"
    , IndexerSearchCompleted
        { searchIndexerName = "nzbindexer"
        , searchResultCount = 17
        , searchDuration = 1.5
        }
    , object
        [ "indexer_name" .= ("nzbindexer" :: Text)
        , "result_count" .= (17 :: Int)
        , "duration" .= (1.5 :: Double)
        ]
    )
  , ( "AlbumSearchCompleted"
    , AlbumSearchCompleted
        { searchTotalResults = 17
        , searchBestScore = Just 90
        , searchDuration = 2.5
        }
    , object
        [ "total_results" .= (17 :: Int)
        , "best_score" .= (90 :: Int)
        , "duration" .= (2.5 :: Double)
        ]
    )
  , ( "BestReleaseSelected"
    , BestReleaseSelected
        { selectedTitle = "Release T"
        , selectedIndexer = "torrentindexer"
        , selectedScore = 88
        , selectedSeeders = Just 12
        }
    , object
        [ "title" .= ("Release T" :: Text)
        , "indexer" .= ("torrentindexer" :: Text)
        , "score" .= (88 :: Int)
        , "seeders" .= (12 :: Int)
        ]
    )
  , ( "SlskdSearchCompleted"
    , SlskdSearchCompleted
        { slskdSearchResultCount = 6
        , slskdSearchDuration = 3.25
        , slskdSearchError = Just "timeout"
        }
    , object
        [ "result_count" .= (6 :: Int)
        , "duration" .= (3.25 :: Double)
        , "error" .= ("timeout" :: Text)
        ]
    )
  , ( "SlskdFilesQueued"
    , SlskdFilesQueued
        { slskdQueuedDownloadId = 91
        , slskdQueuedUsername = "user1"
        , slskdQueuedFileCount = 12
        , slskdQueuedTotalSize = 123456789
        }
    , object
        [ "download_id" .= (91 :: Int64)
        , "username" .= ("user1" :: Text)
        , "file_count" .= (12 :: Int)
        , "total_size" .= (123456789 :: Integer)
        ]
    )
  , ( "DownloadQueued"
    , DownloadQueued
        { downloadId = 101
        , downloadTitle = "Download A"
        , downloadClient = "sabnzbd"
        }
    , object
        [ "download_id" .= (101 :: Int64)
        , "download_title" .= ("Download A" :: Text)
        , "download_client" .= ("sabnzbd" :: Text)
        ]
    )
  , ( "DownloadStarted"
    , DownloadStarted { downloadId = 101, downloadTitle = "Download A" }
    , object
        [ "download_id" .= (101 :: Int64)
        , "download_title" .= ("Download A" :: Text)
        ]
    )
  , ( "DownloadProgress"
    , DownloadProgress
        { downloadId = 101
        , downloadTitle = "Download A"
        , downloadProgress = 0.5
        , downloadSizeBytes = Just 2000
        , downloadedBytes = Just 1000
        }
    , object
        [ "download_id" .= (101 :: Int64)
        , "download_title" .= ("Download A" :: Text)
        , "download_progress" .= (0.5 :: Double)
        , "download_size_bytes" .= (2000 :: Integer)
        , "downloaded_bytes" .= (1000 :: Integer)
        ]
    )
  , ( "DownloadCompleted"
    , DownloadCompleted
        { downloadId = 101
        , downloadTitle = "Download A"
        , downloadPath = Just "/downloads/a"
        }
    , object
        [ "download_id" .= (101 :: Int64)
        , "download_title" .= ("Download A" :: Text)
        , "download_path" .= ("/downloads/a" :: Text)
        ]
    )
  , ( "DownloadFailed"
    , DownloadFailed
        { downloadId = 101
        , downloadTitle = "Download A"
        , downloadError = Just "disk full"
        }
    , object
        [ "download_id" .= (101 :: Int64)
        , "download_title" .= ("Download A" :: Text)
        , "download_error" .= ("disk full" :: Text)
        ]
    )
  , ( "DownloadImported"
    , DownloadImported { downloadId = 101, downloadTitle = "Download A" }
    , object
        [ "download_id" .= (101 :: Int64)
        , "download_title" .= ("Download A" :: Text)
        ]
    )
  , ( "MetadataDiffGenerated"
    , MetadataDiffGenerated
        { diffFileId = 111
        , diffFieldName = "title"
        , diffFileValue = Just "old"
        , diffMBValue = Just "new"
        }
    , object
        [ "file_id" .= (111 :: Int64)
        , "field_name" .= ("title" :: Text)
        , "file_value" .= ("old" :: Text)
        , "mb_value" .= ("new" :: Text)
        ]
    )
  , ( "TrackDiffsGenerated"
    , TrackDiffsGenerated { diffTrackId = 121, diffClusterId = 23, diffCount = 4 }
    , object
        [ "track_id" .= (121 :: Int64)
        , "cluster_id" .= (23 :: Int64)
        , "diff_count" .= (4 :: Int)
        ]
    )
  , ( "TracksRematched"
    , TracksRematched { rematchedClusterId = 24, rematchedTrackCount = 13 }
    , object
        [ "cluster_id" .= (24 :: Int64)
        , "track_count" .= (13 :: Int)
        ]
    )
  , ( "MetadataDiffApplied"
    , MetadataDiffApplied { diffId = 131 }
    , object [ "diff_id" .= (131 :: Int64) ]
    )
  , ( "GroupedDiffsApplied"
    , GroupedDiffsApplied { fieldName = "artist", affectedFileCount = 7 }
    , object
        [ "field_name" .= ("artist" :: Text)
        , "affected_file_count" .= (7 :: Int)
        ]
    )
  , ( "MetadataWriteRequested"
    , MetadataWriteRequested { writeDiffIds = [1, 2, 3] }
    , object [ "diff_ids" .= ([1, 2, 3] :: [Int64]) ]
    )
  , ( "MetadataWriteStarted"
    , MetadataWriteStarted { writeTotalChanges = 5 }
    , object [ "total_changes" .= (5 :: Int) ]
    )
  , ( "MetadataWriteProgress"
    , MetadataWriteProgress
        { writeCurrentFile = "b.flac"
        , writeChangesProcessed = 2
        , writeTotalChanges = 5
        }
    , object
        [ "current_file" .= ("b.flac" :: Text)
        , "changes_processed" .= (2 :: Int)
        , "total_changes" .= (5 :: Int)
        ]
    )
  , ( "MetadataWriteCompleted"
    , MetadataWriteCompleted { writeChangesApplied = 4, writeErrors = 1 }
    , object
        [ "changes_applied" .= (4 :: Int)
        , "errors" .= (1 :: Int)
        ]
    )
  , ( "MetadataWriteFailed"
    , MetadataWriteFailed { writeErrorMessage = "permission denied" }
    , object [ "error_message" .= ("permission denied" :: Text) ]
    )
  , ( "StatsUpdated"
    , StatsUpdated
        { totalFiles = 100
        , totalAlbums = 20
        , totalArtists = 10
        , matchedFiles = 80
        , unmatchedFiles = 20
        , metadataAccuracy = 0.8
        , totalDiffs = 15
        , librarySize = 5000
        , totalRuntime = 36000
        , catalogInLibrary = 18
        , catalogWanted = 4
        }
    , object
        [ "total_files" .= (100 :: Int)
        , "total_albums" .= (20 :: Int)
        , "total_artists" .= (10 :: Int)
        , "matched_files" .= (80 :: Int)
        , "unmatched_files" .= (20 :: Int)
        , "metadata_accuracy" .= (0.8 :: Double)
        , "total_diffs" .= (15 :: Int)
        , "library_size" .= (5000 :: Int)
        , "total_runtime" .= (36000 :: Int)
        , "catalog_in_library" .= (18 :: Int)
        , "catalog_wanted" .= (4 :: Int)
        ]
    )
  , ( "AlbumQualityDetected"
    , AlbumQualityDetected
        { qualityAlbumId = 141
        , qualityDetected = "lossless"
        , qualityPrevious = Just "mp3_320"
        }
    , object
        [ "album_id" .= (141 :: Int64)
        , "quality_detected" .= ("lossless" :: Text)
        , "quality_previous" .= ("mp3_320" :: Text)
        ]
    )
  , ( "AlbumUpgradeNeeded"
    , AlbumUpgradeNeeded
        { upgradeAlbumId = 142
        , upgradeCurrentQuality = "mp3_320"
        , upgradeTargetQuality = "lossless"
        , upgradeProfileName = "HiFi"
        }
    , object
        [ "album_id" .= (142 :: Int64)
        , "current_quality" .= ("mp3_320" :: Text)
        , "target_quality" .= ("lossless" :: Text)
        , "profile_name" .= ("HiFi" :: Text)
        ]
    )
  , ( "AlbumQualityMet"
    , AlbumQualityMet
        { metAlbumId = 143
        , metQuality = "lossless"
        , metProfileName = "HiFi"
        }
    , object
        [ "album_id" .= (143 :: Int64)
        , "quality" .= ("lossless" :: Text)
        , "profile_name" .= ("HiFi" :: Text)
        ]
    )
  , ( "QualityProfileCreated"
    , QualityProfileCreated { profileCreatedId = 151, profileCreatedName = "HiFi" }
    , object
        [ "profile_id" .= (151 :: Int64)
        , "profile_name" .= ("HiFi" :: Text)
        ]
    )
  , ( "QualityProfileUpdated"
    , QualityProfileUpdated { profileUpdatedId = 151, profileUpdatedName = "HiFi v2" }
    , object
        [ "profile_id" .= (151 :: Int64)
        , "profile_name" .= ("HiFi v2" :: Text)
        ]
    )
  , ( "QualityProfileDeleted"
    , QualityProfileDeleted { profileDeletedId = 151, profileDeletedName = "HiFi v2" }
    , object
        [ "profile_id" .= (151 :: Int64)
        , "profile_name" .= ("HiFi v2" :: Text)
        ]
    )
  , ( "ConfigUpdated"
    , ConfigUpdated { updatedConfig = object [ "port" .= (8080 :: Int) ] }
    , object [ "port" .= (8080 :: Int) ]
    )
  , ( "ConfigReloadFailed"
    , ConfigReloadFailed { configReloadError = "bad yaml" }
    , object [ "error" .= ("bad yaml" :: Text) ]
    )
  , ( "Heartbeat"
    , Heartbeat
    , object []
    )
  , ( "TaskCreated"
    , TaskCreated
        { taskCreatedId = "task-1"
        , taskCreatedResource = "artist"
        , taskCreatedType = "refresh"
        }
    , object
        [ "task_id" .= ("task-1" :: Text)
        , "resource" .= ("artist" :: Text)
        , "type" .= ("refresh" :: Text)
        ]
    )
  , ( "TaskProgressUpdated"
    , TaskProgressUpdated
        { taskProgressId = "task-1"
        , taskProgressValue = 0.75
        , taskProgressMessage = Just "halfway"
        }
    , object
        [ "task_id" .= ("task-1" :: Text)
        , "progress" .= (0.75 :: Double)
        , "message" .= ("halfway" :: Text)
        ]
    )
  , ( "TaskCompleted"
    , TaskCompleted
        { taskCompletedId = "task-1"
        , taskCompletedResult = Just (object [ "ok" .= True ])
        }
    , object
        [ "task_id" .= ("task-1" :: Text)
        , "result" .= object [ "ok" .= True ]
        ]
    )
  , ( "TaskFailed'"
    , TaskFailed' { taskFailedId = "task-2", taskFailedError = "boom" }
    , object
        [ "task_id" .= ("task-2" :: Text)
        , "error" .= ("boom" :: Text)
        ]
    )
  , ( "TaskCancelled'"
    , TaskCancelled' { taskCancelledId = "task-3" }
    , object [ "task_id" .= ("task-3" :: Text) ]
    )
  ]

goldenPayloadTests :: TestTree
goldenPayloadTests = testGroup "eventToJSON golden payloads"
  [ testCase (toString name) $ do
      assertEqual "event type" name (eventType evt)
      assertEqual "payload" expected (eventToJSON evt)
  | (name, evt, expected) <- goldenCases
  ]

-- | Every constructor of 'Event' must have a golden case above; adding a
-- new event without pinning its wire format fails here.
constructorCompletenessTest :: TestTree
constructorCompletenessTest = testCase "every Event constructor has a golden case" $ do
  let allConstrs = sort (map (toText . showConstr) (dataTypeConstrs (dataTypeOf Heartbeat)))
      covered = sort (map (\(name, _, _) -> name) goldenCases)
  assertEqual "constructors with golden cases" allConstrs covered

envelopeTest :: TestTree
envelopeTest = testCase "EventEnvelope carries metadata, type and data" $ do
  let meta = EventMetadata
        { eventId = UUID.nil
        , eventTimestamp = UTCTime (fromGregorian 2026 7 25) 0
        , eventSource = "test"
        , eventSequence = 42
        }
      expected = object
        [ "metadata" .= object
            [ "id" .= ("00000000-0000-0000-0000-000000000000" :: Text)
            , "timestamp" .= UTCTime (fromGregorian 2026 7 25) 0
            , "source" .= ("test" :: Text)
            , "sequence" .= (42 :: Int64)
            ]
        , "type" .= ("Heartbeat" :: Text)
        , "data" .= object []
        ]
  assertEqual "envelope" expected (toJSON (EventEnvelope meta Heartbeat))
