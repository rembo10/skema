{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Importer service - imports completed downloads into the library.
--
-- This service:
-- 1. Listens for DownloadCompleted events
-- 2. Scans downloaded files and extracts metadata
-- 3. Identifies the release against MusicBrainz
-- 4. Formats the library path using templates
-- 5. Creates cluster and library_tracks records directly
-- 6. Moves files to the library
-- 7. Links download → cluster → catalog_album
-- 8. Marks download as imported
--
-- Unlike the scanner+grouper+identifier workflow (which is event-driven),
-- the importer does everything synchronously for faster import times.
module Skema.Services.Importer
  ( startImporterService
  ) where

import Skema.Services.Dependencies (ImporterDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.Database.Types (DownloadRecord(..), CatalogAlbumRecord(..))
import qualified Skema.Database.Types as DB
import Skema.Database.Utils (downloadStatusToText)
import Skema.Domain.Quality (qualityToText, textToQuality, meetsOrExceedsCutoff)
import Skema.FileSystem.PathFormatter (PathContext(..), formatPath)
import Skema.FileSystem.Utils (moveFile, osPathToString, stringToOsPath)
import Skema.FileSystem.Trash (moveToTrash)
import Skema.Core.Metadata (scanAndParseMetadata, groupParsedFiles, MetadataResult(..), GroupedFiles(..))
import Skema.MusicBrainz.Identify (identifyFileGroup)
import Skema.MusicBrainz.Types (FileGroup(..), ReleaseMatch(..), IdentificationResult(..), MBRelease(..), MBID(..), unMBID, mbSearchReleases, TrackMatch(..), MBTrack(..), mbReleaseTracks)
import Skema.MusicBrainz.Client (MBClientEnv, searchReleases)
import Skema.Domain.Identification (IdentifyConfig(..), buildSearchQuery)
import Skema.Config.Types (Config(..), LibraryConfig(..), DownloadConfig(..), download)
import qualified Monatone.Metadata as M
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try, throwIO)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite
import System.OsPath ((</>))
import qualified System.OsPath as OP
import qualified System.Directory as Dir
import System.FilePath (takeExtension)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Katip

-- | Exception type for import failures.
-- Provides clean error messages without CallStack noise.
newtype ImportException = ImportException Text
  deriving (Show)

instance Exception ImportException

-- | Start the importer service.
--
-- Listens for DownloadCompleted events and imports them into the library.
startImporterService :: ImporterDeps -> IO (Async ())
startImporterService deps = do
  chan <- STM.atomically $ subscribe (impEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      DownloadCompleted{..} -> do
        -- Process each import asynchronously
        _ <- async $ do
          result <- try $ handleDownloadCompleted deps downloadId
          case result of
            Left (e :: SomeException) -> do
              let le = impLogEnv deps
              runKatipContextT le () "importer" $ do
                $(logTM) ErrorS $ logStr $ ("Exception importing download: " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle a download completed event by importing it into the library.
handleDownloadCompleted :: ImporterDeps -> Int64 -> IO ()
handleDownloadCompleted ImporterDeps{..} downloadId = do
  let le = impLogEnv
  let pool = impDbPool
  let bus = impEventBus
  let initialContext = ()
  let initialNamespace = "importer"

  -- Read current config
  config <- STM.atomically $ STM.readTVar impConfigVar

  runKatipContextT le initialContext initialNamespace $ do
    -- Get download record
    downloads <- liftIO $ withConnection pool $ \conn ->
      queryRows conn
        "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
        \download_client_id, status, download_path, title, size_bytes, quality, \
        \format, seeders, progress, error_message, queued_at, started_at, \
        \completed_at, imported_at, updated_at, \
        \matched_cluster_id, library_path \
        \FROM downloads WHERE id = ?"
        (Only downloadId) :: IO [DownloadRecord]

    case viaNonEmpty head downloads of
      Nothing -> do
        $(logTM) ErrorS $ logStr $ ("Download not found: " <> show downloadId :: Text)

      Just downloadRec -> do
        case DB.downloadPath downloadRec of
          Nothing -> do
            $(logTM) ErrorS $ logStr $ ("Completed download has no path: " <> DB.downloadTitle downloadRec :: Text)
            liftIO $ markDownloadAsFailed pool downloadId "No download path"

          Just downloadPathText | T.null (T.strip downloadPathText) -> do
            $(logTM) ErrorS $ logStr $ ("Download has empty path: " <> DB.downloadTitle downloadRec :: Text)
            liftIO $ markDownloadAsFailed pool downloadId "Download path is empty"

          Just _downloadPathText -> do
            $(logTM) InfoS $ logStr $ ("Importing download: " <> DB.downloadTitle downloadRec :: Text)

            -- Get catalog album
            let catalogAlbumId = DB.downloadCatalogAlbumId downloadRec
            catalogAlbums <- liftIO $ withConnection pool $ \conn ->
              getCatalogAlbumById conn catalogAlbumId

            case catalogAlbums of
              Nothing -> do
                $(logTM) ErrorS $ logStr ("Catalog album not found for download" :: Text)
                liftIO $ markDownloadAsFailed pool downloadId "Catalog album not found"

              Just catalogAlbum -> do
                -- Import the download
                importResult <- liftIO $ try $ importDownload config le bus pool impMBClient downloadRec catalogAlbum
                case importResult of
                  Left (e :: SomeException) -> do
                    let errorMsg = case fromException e of
                          Just (ImportException msg) -> msg
                          Nothing -> show e
                    $(logTM) ErrorS $ logStr $ ("Import failed: " <> errorMsg :: Text)
                    liftIO $ markDownloadAsFailed pool downloadId errorMsg

                  Right () -> pure ()  -- Success already logged in importDownload

-- | Import a download into the library.
importDownload :: Config -> LogEnv -> EventBus -> ConnectionPool -> MBClientEnv -> DownloadRecord -> CatalogAlbumRecord -> IO ()
importDownload config le bus pool mbClientEnv downloadRec catalogAlbum = do
  let downloadPathText = fromMaybe "" (DB.downloadPath downloadRec)

  runKatipContextT le () "importer" $ do
    -- 1. Parse download path to OsPath
    downloadPath <- liftIO $ stringToOsPath (toString downloadPathText)
    $(logTM) InfoS $ logStr $ ("Scanning download directory: " <> downloadPathText :: Text)

    -- 2. Scan and parse metadata from all files (shared with Scanner)
    metadataResult <- liftIO $ scanAndParseMetadata downloadPath

    let validFiles = mrValidFiles metadataResult
        failedCount = length (mrFailedFiles metadataResult)
        totalFiles = mrTotalFiles metadataResult

    $(logTM) InfoS $ logStr $ ("Found " <> show totalFiles <> " files in download directory" :: Text)

    -- Log parse failures for debugging
    unless (null (mrFailedFiles metadataResult)) $ do
      forM_ (mrFailedFiles metadataResult) $ \(path, err) -> do
        pathStr <- liftIO $ osPathToString path
        $(logTM) DebugS $ logStr $ ("Failed to parse metadata: " <> toText pathStr <> " - " <> toText err :: Text)

    when (null validFiles) $ do
      $(logTM) ErrorS $ logStr $ ("No valid audio files found in " <> downloadPathText <>
                                  ": scanned " <> show totalFiles <>
                                  " files, all failed metadata parsing" :: Text)
      liftIO $ throwIO $ ImportException $ "No valid audio files found in download directory '" <> downloadPathText <>
              "'. Scanned " <> show totalFiles <>
              " files but none had parseable audio metadata. Check if files are corrupt or in unsupported formats."

    $(logTM) InfoS $ logStr $ ("Found " <> show (length validFiles) <> " valid audio files" <>
                               (if failedCount > 0
                                then " (" <> show failedCount <> " files failed to parse)"
                                else "") :: Text)

    -- 3. Group files by album (shared with Scanner/Grouper)
    grouped <- liftIO $ groupParsedFiles validFiles
    let fileGroups = gfGroups grouped
    $(logTM) DebugS $ logStr $ ("File grouping produced " <> show (length fileGroups) <> " groups" :: Text)

    case viaNonEmpty head fileGroups of
      Nothing -> do
        $(logTM) ErrorS $ logStr ("Could not group files into album: no consistent album metadata found across files" :: Text)
        liftIO $ throwIO $ ImportException "Could not group files into album. The audio files may have inconsistent or missing album/artist metadata."

      Just fileGroup -> do
        let albumName = fromMaybe "<unknown>" (fgAlbum fileGroup)
        let artistName = fromMaybe "<unknown>" (fgArtist fileGroup)
        let trackCount = length (fgFiles fileGroup)

        $(logTM) InfoS $ logStr $ ("Grouped into album: " <> albumName :: Text)
        $(logTM) InfoS $ logStr $ ("Album artist: " <> artistName :: Text)
        $(logTM) InfoS $ logStr $ ("Track count: " <> show trackCount :: Text)

        -- 5. Identify against MusicBrainz (reuse identifier)
        let libConfig = library config
        let identifyConfig = IdentifyConfig
              { cfgMaxCandidates = 5
              , cfgMinConfidence = 0.35
              , cfgSearchLimit = 20
              , cfgNormalizeFeaturing = libraryNormalizeFeaturing libConfig
              , cfgNormalizeFeaturingTo = libraryNormalizeFeaturingTo libConfig
              , cfgRetryIntervalHours = 24
              }

        -- Log the search query that will be used
        let searchQuery = buildSearchQuery fileGroup
        $(logTM) InfoS $ logStr $ ("MusicBrainz search query: " <> searchQuery :: Text)
        $(logTM) InfoS $ logStr $ ("Minimum confidence threshold: " <> show (cfgMinConfidence identifyConfig) :: Text)

        $(logTM) InfoS $ logStr ("Identifying release with MusicBrainz..." :: Text)

        -- Try identification and capture detailed info for diagnostics
        identifyResult <- liftIO $ do
          -- First search MusicBrainz to see what we get
          let query = buildSearchQuery fileGroup
          searchResult <- searchReleases mbClientEnv query (Just $ cfgSearchLimit identifyConfig) Nothing

          case searchResult of
            Left err -> do
              -- Log search failure details
              pure $ Left err

            Right mbSearchResult -> do
              let searchCandidates = mbSearchReleases mbSearchResult
                  candidateCount = length searchCandidates

              -- Now do the full identification (temporarily with 0.0 threshold to see all scores)
              -- We'll manually check the threshold and provide detailed feedback
              let debugConfig = identifyConfig { cfgMinConfidence = 0.0 }
              fullResult <- identifyFileGroup le mbClientEnv debugConfig fileGroup

              pure $ case fullResult of
                Left err -> Left err
                Right (IdentificationResult maybeMatch _) -> Right (candidateCount, maybeMatch, cfgMinConfidence identifyConfig)

        case identifyResult of
          Left err -> do
            $(logTM) ErrorS $ logStr $ ("MusicBrainz API error: " <> show err :: Text)
            $(logTM) ErrorS $ logStr $ ("Album: " <> albumName <>
                                        ", Artist: " <> artistName :: Text)
            let errorMsg = "MusicBrainz identification failed: " <> show err
            liftIO $ markDownloadAsIdentificationFailure pool (fromMaybe 0 (DB.downloadId downloadRec)) errorMsg
            $(logTM) InfoS $ logStr ("Download marked as identification_failure - can be retried later" :: Text)

          Right (candidateCount, Nothing, _) -> do
            $(logTM) ErrorS $ logStr $ ("No MusicBrainz match found for album: " <> albumName <>
                                        " by " <> artistName :: Text)
            $(logTM) InfoS $ logStr $ ("MusicBrainz returned " <> show candidateCount <> " search results" :: Text)
            $(logTM) ErrorS $ logStr ("Album not found in MusicBrainz database or no candidates could be fetched" :: Text)
            let errorMsg = "No MusicBrainz match found (" <> show candidateCount <> " candidates searched). The release may not be in MusicBrainz database, or no candidate details could be fetched."
            liftIO $ markDownloadAsIdentificationFailure pool (fromMaybe 0 (DB.downloadId downloadRec)) errorMsg
            $(logTM) InfoS $ logStr ("Download marked as identification_failure - can be retried later" :: Text)

          Right (candidateCount, Just match, minConfidence) -> do
            $(logTM) InfoS $ logStr $ ("MusicBrainz search found " <> show candidateCount <> " candidates" :: Text)
            let release = rmRelease match
            let releaseMBID = unMBID (mbReleaseId release)
            let confidence = rmConfidence match
            let _allCandidates = rmCandidates match
            let fileTrackCount = length (fgFiles fileGroup)
            let trackMatches = rmTrackMatches match

            -- Log the best match info
            $(logTM) InfoS $ logStr $ ("Best match: " <> mbReleaseTitle release <>
                                       " (MBID: " <> releaseMBID <> ")" :: Text)
            $(logTM) InfoS $ logStr $ ("Confidence: " <> show (round (confidence * 100) :: Integer) <>
                                       "% (threshold: " <> show (round (minConfidence * 100) :: Integer) <> "%)" :: Text)
            $(logTM) InfoS $ logStr $ ("File track count: " <> show fileTrackCount <>
                                       ", Release track count: " <> show (length $ mbReleaseTracks release) :: Text)

            -- Log detailed track matching information
            $(logTM) InfoS $ logStr ("=== Track Matching Details ===" :: Text)
            $(logTM) InfoS $ logStr $ ("Matched " <> show (length trackMatches) <> " tracks" :: Text)

            -- Build a map of file paths to metadata for quick lookup
            let fileMetaMap = Map.fromList validFiles

            -- Show file tracks vs MB tracks
            forM_ (zip [1..] trackMatches) $ \(idx :: Int, tm) -> do
              let filePath = tmFilePath tm
              let mbTrack = tmTrack tm
              let cost = tmCost tm
              let matchQuality = round ((1.0 - cost) * 100) :: Integer

              -- Look up the file metadata
              case Map.lookup filePath fileMetaMap of
                Just fileMeta -> do
                  let fileTitle = fromMaybe "<no title>" (M.title fileMeta)
                  let fileArtist = fromMaybe "" (M.artist fileMeta)
                  let mbTitle = mbTrackTitle mbTrack
                  let mbArtist = fromMaybe "" (mbTrackArtist mbTrack)

                  $(logTM) InfoS $ logStr $ ("  Track " <> show idx <> ": [" <> show matchQuality <> "%]" :: Text)
                  $(logTM) InfoS $ logStr $ ("    File:  \"" <> fileTitle <> "\" by " <> fileArtist :: Text)
                  $(logTM) InfoS $ logStr $ ("    MB:    \"" <> mbTitle <> "\" by " <> mbArtist :: Text)
                Nothing -> do
                  $(logTM) InfoS $ logStr $ ("  Track " <> show idx <> ": [" <> show matchQuality <> "%] (file metadata not found)" :: Text)

            $(logTM) InfoS $ logStr ("==============================" :: Text)

            -- Check if confidence meets threshold
            if confidence < minConfidence
              then do
                $(logTM) ErrorS $ logStr $ ("Confidence too low: " <> show (round (confidence * 100) :: Integer) <>
                                            "% < " <> show (round (minConfidence * 100) :: Integer) <> "% threshold" :: Text)
                $(logTM) InfoS $ logStr ("Suggestion: Lower the confidence threshold in config or check if track titles/counts match" :: Text)
                let errorMsg = "Best match confidence (" <> show (round (confidence * 100) :: Integer) <>
                               "%) is below threshold (" <> show (round (minConfidence * 100) :: Integer) <> "%)."
                liftIO $ markDownloadAsIdentificationFailure pool (fromMaybe 0 (DB.downloadId downloadRec)) errorMsg
                $(logTM) InfoS $ logStr ("Download marked as identification_failure - can be retried later" :: Text)
              else do
                -- Confidence meets threshold, proceed with import
                $(logTM) InfoS $ logStr $ ("✓ Identified as: " <> mbReleaseTitle release <>
                                           " (confidence: " <> show (round (confidence * 100) :: Integer) <> "%)" :: Text)

                -- 6. Extract release metadata from MusicBrainz data for path formatting
                let releaseMetadata = extractReleaseMetadata release

                -- 7. Format library path using templates
                libraryBasePath <- case libraryPath libConfig of
                  Nothing -> do
                    $(logTM) ErrorS $ logStr ("No library path configured in settings" :: Text)
                    liftIO $ throwIO $ ImportException "No library path configured. Please set the library path in your configuration before importing downloads."
                  Just path -> pure path

                -- Build PathContext for directory formatting
                let albumArtist = DB.catalogAlbumArtistName catalogAlbum
                let albumTitle = DB.catalogAlbumTitle catalogAlbum
                let year = extractYear (DB.catalogAlbumFirstReleaseDate catalogAlbum)
                let pathContext = PathContext
                      { pcAlbumArtist = albumArtist
                      , pcAlbum = albumTitle
                      , pcYear = year
                      , pcTotalDiscs = rmDiscCount releaseMetadata
                      , pcTotalTracks = length validFiles  -- Approximate
                      , pcCountry = rmCountry releaseMetadata
                      , pcLabel = rmLabel releaseMetadata
                      , pcCatalogNumber = rmCatalogNumber releaseMetadata
                      , pcArtist = Nothing
                      , pcTitle = Nothing
                      , pcDisc = Nothing
                      , pcTrack = Nothing
                      , pcFormat = Nothing
                      , pcBitrate = Nothing
                      , pcSampleRate = Nothing
                      , pcBitDepth = Nothing
                      , pcExtension = Nothing
                      }

                -- Format directory path from template
                let dirPath = formatPath (libraryPathFormat libConfig) pathContext
                targetDir <- liftIO $ do
                  dirOsPath <- stringToOsPath (toString dirPath)
                  pure $ libraryBasePath </> dirOsPath

                $(logTM) InfoS $ logStr $ ("Target directory: " <> show targetDir :: Text)

                -- 9. Create cluster and library_tracks directly
                clusterId <- liftIO $ createClusterFromMatch pool fileGroup match

                $(logTM) InfoS $ logStr $ ("Created cluster with ID: " <> show clusterId :: Text)

                -- 10. Delete old files if this is an upgrade and replace_library_files is enabled
                let downloadCfg = download config
                when (downloadReplaceLibraryFiles downloadCfg) $ do
                  -- Check if there's an existing cluster for this release group (indicating an upgrade)
                  forM_ (mbReleaseGroupId (rmRelease match)) $ \releaseGroupMBID -> do
                    existingClusters <- liftIO $ withConnection pool $ \conn ->
                      queryRows conn
                        "SELECT id FROM clusters WHERE mb_release_group_id = ? AND id != ?"
                        (unMBID releaseGroupMBID, clusterId) :: IO [Only Int64]

                    case viaNonEmpty head existingClusters of
                      Just (Only existingClusterId) -> do
                        $(logTM) InfoS $ logStr $ ("Upgrading album - removing old files from cluster " <> show existingClusterId :: Text)

                        -- Get all tracks from the existing cluster
                        existingTracks <- liftIO $ withConnection pool $ \conn ->
                          queryRows conn
                            "SELECT file_path FROM library_tracks WHERE cluster_id = ?"
                            (Only existingClusterId) :: IO [Only Text]

                        -- Delete or trash each old file based on config
                        forM_ existingTracks $ \(Only trackPathText) -> do
                          trackPath <- liftIO $ stringToOsPath (toString trackPathText)
                          trackPathStr <- liftIO $ osPathToString trackPath
                          fileExists <- liftIO $ Dir.doesFileExist trackPathStr
                          when fileExists $ do
                            if downloadUseTrash downloadCfg
                              then do
                                $(logTM) InfoS $ logStr $ ("Moving old file to trash: " <> trackPathText :: Text)
                                liftIO $ moveToTrash libraryBasePath trackPath
                              else do
                                $(logTM) InfoS $ logStr $ ("Permanently deleting old file: " <> trackPathText :: Text)
                                liftIO $ Dir.removeFile trackPathStr

                        -- Delete track records for old cluster
                        liftIO $ withConnection pool $ \conn ->
                          executeQuery conn
                            "DELETE FROM library_tracks WHERE cluster_id = ?"
                            (Only existingClusterId)

                        -- Delete the old cluster
                        liftIO $ withConnection pool $ \conn ->
                          executeQuery conn
                            "DELETE FROM clusters WHERE id = ?"
                            (Only existingClusterId)

                        if downloadUseTrash downloadCfg
                          then $(logTM) InfoS $ logStr $ ("Old files moved to trash (will be deleted after " <> show (downloadTrashRetentionDays downloadCfg) <> " days)" :: Text)
                          else $(logTM) InfoS $ logStr ("Old files permanently deleted" :: Text)

                      Nothing -> do
                        $(logTM) DebugS $ logStr ("Not an upgrade - this is the first download for this album" :: Text)

                -- 11. Move files to library and create track records with formatted filenames
                $(logTM) InfoS $ logStr ("Moving files to library..." :: Text)

                -- Create target directory
                liftIO $ do
                  targetDirStr <- osPathToString targetDir
                  Dir.createDirectoryIfMissing True targetDirStr

                -- 12. Move each file with formatted filename and create track records
                -- Build a map from file paths to MusicBrainz tracks
                let trackMatchMap = Map.fromList [(tmFilePath tm, tmTrack tm) | tm <- rmTrackMatches match]

                trackIds <- liftIO $ forM validFiles $ \(oldPath, meta) -> do
                  -- Extract file extension (without leading dot)
                  let oldFileName = OP.takeFileName oldPath
                  oldFileNameStr <- osPathToString oldFileName
                  let extension = toText $ drop 1 $ takeExtension oldFileNameStr

                  -- Look up the matched MusicBrainz track for this file
                  let mbTrack = Map.lookup oldPath trackMatchMap

                  -- Build track-specific PathContext using MusicBrainz data when available
                  let trackContext = pathContext
                        { pcArtist = maybe (M.artist meta) mbTrackArtist mbTrack
                        , pcTitle = maybe (M.title meta) (Just . mbTrackTitle) mbTrack
                        , pcDisc = M.discNumber meta  -- Keep file metadata for disc (not in MBTrack)
                        , pcTrack = maybe (M.trackNumber meta) (Just . mbTrackPosition) mbTrack
                        , pcFormat = Nothing  -- TODO: Extract format from metadata
                        , pcBitrate = Nothing  -- TODO: Extract bitrate from metadata
                        , pcSampleRate = Nothing  -- Not available in monatone
                        , pcBitDepth = Nothing  -- Not available in monatone
                        , pcExtension = Just extension
                        }

                  -- Format filename from template
                  let formattedFileName = formatPath (libraryFileFormat libConfig) trackContext
                  formattedFileNameOsPath <- stringToOsPath (toString formattedFileName)
                  let newPath = targetDir </> formattedFileNameOsPath

                  -- Move file to new path
                  sourceStr <- osPathToString oldPath
                  targetStr <- osPathToString newPath
                  moveFile sourceStr targetStr

                  -- Insert/update track record
                  withConnection pool $ \conn -> do
                    upsertTrackWithMetadata conn newPath meta

                -- Link all tracks to cluster
                let validTrackIds = trackIds
                liftIO $ withConnection pool $ \conn -> do
                  updateTrackCluster conn clusterId validTrackIds
                  -- Compute and store cluster quality from track audio metadata
                  updateClusterQuality conn clusterId

                -- 13. Link download to cluster and set library path
                liftIO $ withConnection pool $ \conn ->
                  executeQuery conn
                    "UPDATE downloads SET matched_cluster_id = ?, library_path = ? WHERE id = ?"
                    (clusterId, dirPath, DB.downloadId downloadRec)

                -- 14. Get cluster quality (already computed and stored)
                maybeCluster <- liftIO $ withConnection pool $ \conn ->
                  getClusterById conn clusterId

                let maybeQualityText = maybeCluster >>= DB.clusterQuality
                let maybeQuality = maybeQualityText >>= textToQuality

                -- Update catalog_albums.current_quality from cluster
                liftIO $ withConnection pool $ \conn ->
                  executeQuery conn
                    "UPDATE catalog_albums SET current_quality = ?, updated_at = CURRENT_TIMESTAMP \
                    \WHERE release_group_mbid = (SELECT mb_release_group_id FROM clusters WHERE id = ?)"
                    (maybeQualityText, clusterId)

                -- The current_quality is already updated above (step 14)
                -- The wanted status will be automatically derived from:
                -- quality_profile_id + current_quality + matched_cluster_id
                -- The acquisition system will check if upgrades are still needed

                -- 15. Mark download as imported
                now <- liftIO getCurrentTime
                liftIO $ withConnection pool $ \conn ->
                  executeQuery conn
                    "UPDATE downloads SET status = ?, imported_at = ? WHERE id = ?"
                    (downloadStatusToText DB.DownloadImported, now, DB.downloadId downloadRec)

                -- 16. Emit DownloadImported event
                liftIO $ publishAndLog bus le "importer" $ DownloadImported
                  { downloadId = fromMaybe 0 (DB.downloadId downloadRec)
                  , downloadTitle = DB.downloadTitle downloadRec
                  }

                $(logTM) InfoS $ logStr ("Import completed successfully" :: Text)

-- | Extract release metadata from MusicBrainz release for path formatting.
data ReleaseMetadata = ReleaseMetadata
  { rmLabel :: Maybe Text
  , rmCatalogNumber :: Maybe Text
  , rmCountry :: Maybe Text
  , rmDiscCount :: Int
  } deriving (Show)

extractReleaseMetadata :: MBRelease -> ReleaseMetadata
extractReleaseMetadata release = ReleaseMetadata
  { rmLabel = mbReleaseLabel release
  , rmCatalogNumber = mbReleaseCatalogNumber release
  , rmCountry = mbReleaseCountry release
  , rmDiscCount = 1  -- TODO: Calculate from tracks
  }

-- | Create a cluster from a MusicBrainz match.
-- Inserts the cluster and returns the cluster ID.
createClusterFromMatch :: ConnectionPool -> FileGroup -> ReleaseMatch -> IO Int64
createClusterFromMatch pool fileGroup match = do
  let release = rmRelease match
  let album = fgAlbum fileGroup
  let albumArtist = fgArtist fileGroup
  let trackCount = length (fgFiles fileGroup)
  let releaseMBID = unMBID (mbReleaseId release)
  let releaseGroupMBID = fmap unMBID (mbReleaseGroupId release)
  let confidence = rmConfidence match

  withConnection pool $ \conn -> do
    -- Compute cluster hash
    let hash = computeClusterHash album albumArtist trackCount

    -- Check if cluster already exists
    maybeCluster <- findClusterByHash conn hash

    case maybeCluster of
      Just cluster -> pure $ fromMaybe 0 (DB.clusterId cluster)
      Nothing -> do
        -- Create new cluster with MusicBrainz data
        clusterId <- createCluster conn hash album albumArtist trackCount

        -- Update with MusicBrainz data
        executeQuery conn
          "UPDATE clusters SET mb_release_id = ?, mb_release_group_id = ?, mb_confidence = ? WHERE id = ?"
          (Just releaseMBID, releaseGroupMBID, Just confidence, clusterId)

        pure clusterId

-- | Extract year from date string (YYYY-MM-DD or YYYY).
extractYear :: Maybe Text -> Text
extractYear Nothing = "Unknown"
extractYear (Just dateStr) =
  case T.take 4 dateStr of
    year | T.length year == 4 -> year
    _ -> "Unknown"

-- | Mark a download as failed with an error message.
markDownloadAsFailed :: ConnectionPool -> Int64 -> Text -> IO ()
markDownloadAsFailed pool downloadId errorMsg = do
  withConnection pool $ \conn ->
    executeQuery conn
      "UPDATE downloads SET status = ?, error_message = ? WHERE id = ?"
      (downloadStatusToText DB.DownloadFailed, errorMsg, downloadId)

-- | Mark a download as having identification failure with an error message.
-- This status indicates MusicBrainz matching failed, but the download can be retried.
markDownloadAsIdentificationFailure :: ConnectionPool -> Int64 -> Text -> IO ()
markDownloadAsIdentificationFailure pool downloadId errorMsg = do
  withConnection pool $ \conn ->
    executeQuery conn
      "UPDATE downloads SET status = ?, error_message = ? WHERE id = ?"
      (downloadStatusToText DB.DownloadIdentificationFailure, errorMsg, downloadId)

-- Helper to get catalog album by ID
getCatalogAlbumById :: SQLite.Connection -> Int64 -> IO (Maybe CatalogAlbumRecord)
getCatalogAlbumById conn albumId = do
  albums <- queryRows conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, \
    \album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, \
    \quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums WHERE id = ?"
    (Only albumId) :: IO [CatalogAlbumRecord]
  pure $ viaNonEmpty head albums

