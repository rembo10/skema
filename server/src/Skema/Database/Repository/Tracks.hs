{-# LANGUAGE OverloadedStrings #-}

-- | Track and metadata repository operations.
module Skema.Database.Repository.Tracks
  ( -- * Helper types
    MetadataInsert(..)
    -- * Helper functions
  , osPathToString
  , stringToOsPath
    -- * Track operations
  , insertTrack
  , updateTrack
  , deleteTrack
  , getTrackByPath
  , getAllTracks
  , getLibrarySnapshot
    -- * Metadata operations
  , insertTrackMetadata
  , updateTrackMetadata
  , getMetadataForTrack
  , upsertTrackWithMetadata
    -- * MusicBrainz operations
  , updateMusicBrainzIds
  , getMusicBrainzIdsByPath
    -- * Scan history
  , insertScanHistory
  , updateScanHistory
  , getRecentScans
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Core.Library (LibrarySnapshot(..), FileInfo(..))
import Skema.Domain.Quality (detectQualityFromAudio, qualityToText)
import Monatone.Metadata (Metadata(..), AudioFormat(..))
import qualified Monatone.Metadata as M
import Control.Monad (foldM)
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import System.Directory.OsPath (getFileSize, getModificationTime)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple.ToRow as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Map.Strict as Map
import GHC.IO.Encoding (mkTextEncoding)
import Control.Exception (throwIO)

-- * Helper types

-- | Helper type for inserting metadata with more than 10 fields
data MetadataInsert = MetadataInsert
  { miTrackId :: Int64
  , miTitle :: Maybe Text
  , miArtist :: Maybe Text
  , miAlbum :: Maybe Text
  , miAlbumArtist :: Maybe Text
  , miTrackNumber :: Maybe Int
  , miDiscNumber :: Maybe Int
  , miYear :: Maybe Int
  , miDurationSeconds :: Maybe Double
  , miGenre :: Maybe Text
  , miDate :: Maybe Text
  , miBarcode :: Maybe Text
  , miCatalogNumber :: Maybe Text
  , miLabel :: Maybe Text
  , miCountry :: Maybe Text
  , miMBRecordingId :: Maybe Text
  , miMBTrackId :: Maybe Text
  , miMBReleaseId :: Maybe Text
  , miMBReleaseGroupId :: Maybe Text
  , miMBArtistId :: Maybe Text
  , miFormat :: Maybe Text
  , miBitrate :: Maybe Int
  , miSampleRate :: Maybe Int
  , miBitsPerSample :: Maybe Int
  }

-- | SQLite ToRow instance for MetadataInsert
instance SQLite.ToRow MetadataInsert where
  toRow m = [ SQLite.toField (miTrackId m)
            , SQLite.toField (miTitle m)
            , SQLite.toField (miArtist m)
            , SQLite.toField (miAlbum m)
            , SQLite.toField (miAlbumArtist m)
            , SQLite.toField (miTrackNumber m)
            , SQLite.toField (miDiscNumber m)
            , SQLite.toField (miYear m)
            , SQLite.toField (miDurationSeconds m)
            , SQLite.toField (miGenre m)
            , SQLite.toField (miDate m)
            , SQLite.toField (miBarcode m)
            , SQLite.toField (miCatalogNumber m)
            , SQLite.toField (miLabel m)
            , SQLite.toField (miCountry m)
            , SQLite.toField (miMBRecordingId m)
            , SQLite.toField (miMBTrackId m)
            , SQLite.toField (miMBReleaseId m)
            , SQLite.toField (miMBReleaseGroupId m)
            , SQLite.toField (miMBArtistId m)
            , SQLite.toField (miFormat m)
            , SQLite.toField (miBitrate m)
            , SQLite.toField (miSampleRate m)
            , SQLite.toField (miBitsPerSample m)
            ]

-- * Helper functions

-- | Convert AudioFormat to Text for database storage.
audioFormatToText :: AudioFormat -> Text
audioFormatToText FLAC = "flac"
audioFormatToText OGG = "ogg"
audioFormatToText Opus = "opus"
audioFormatToText MP3 = "mp3"
audioFormatToText M4A = "m4a"

-- | Convert OsPath to String for database storage.
-- Uses UTF-8 with ROUNDTRIP mode (PEP 383) to preserve exact byte sequences,
-- even for invalid UTF-8. This ensures we can always reconstruct the original path.
osPathToString :: OsPath -> IO String
osPathToString path = do
  enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
  case OP.decodeWith enc enc path of
    Left err -> throwIO err
    Right str -> pure str

-- | Convert String to OsPath from database.
-- Uses UTF-8 with ROUNDTRIP mode (PEP 383) to preserve exact byte sequences,
-- even for invalid UTF-8. This ensures we can always reconstruct the original path.
stringToOsPath :: String -> IO OsPath
stringToOsPath str = do
  enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
  case OP.encodeWith enc enc str of
    Left err -> throwIO err
    Right path -> pure path


-- * Track operations

-- | Insert a new track record.
insertTrack :: SQLite.Connection -> OsPath -> Integer -> UTCTime -> IO (Maybe Int64)
insertTrack conn path size modTime = do
  pathStr <- osPathToString path
  executeQuery conn
    "INSERT INTO library_tracks (path, size, modified_at) VALUES (?, ?, ?)"
    (pathStr :: String, size, modTime)
  -- TODO: Return the inserted ID
  pure Nothing

-- | Update an existing track record.
updateTrack :: SQLite.Connection -> Int64 -> Integer -> UTCTime -> IO ()
updateTrack conn tid size modTime =
  executeQuery conn
    "UPDATE library_tracks SET size = ?, modified_at = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?"
    (size, modTime, tid)

-- | Delete a track record.
deleteTrack :: SQLite.Connection -> Int64 -> IO ()
deleteTrack conn tid =
  executeQuery conn "DELETE FROM library_tracks WHERE id = ?" (Only tid)

-- | Get track by path.
getTrackByPath :: SQLite.Connection -> OsPath -> IO (Maybe LibraryTrackRecord)
getTrackByPath conn path = do
  pathStr <- OP.decodeUtf path
  results <- queryRows conn
    "SELECT id, path, size, modified_at, cluster_id, mb_recording_id, mb_track_id, mb_release_id, mb_confidence, added_at, updated_at FROM library_tracks WHERE path = ?"
    (Only (pathStr :: String))
  pure $ viaNonEmpty head results

-- | Get all tracks in library.
getAllTracks :: SQLite.Connection -> IO [LibraryTrackRecord]
getAllTracks conn =
  queryRows_ conn "SELECT id, path, size, modified_at, cluster_id, mb_recording_id, mb_track_id, mb_release_id, mb_confidence, added_at, updated_at FROM library_tracks ORDER BY path"

-- | Get a library snapshot from the database.
-- This loads paths and mtimes from the database to create a LibrarySnapshot
-- that can be compared with the filesystem state.
getLibrarySnapshot :: SQLite.Connection -> IO LibrarySnapshot
getLibrarySnapshot conn = do
  -- Query path, size, and modified_at for all tracks
  results <- queryRows_ conn
    "SELECT path, size, modified_at FROM library_tracks" :: IO [(String, Integer, UTCTime)]

  -- Convert to Map of OsPath -> FileInfo
  fileMap <- foldM (\acc (pathStr, size, modTime) -> do
    path <- stringToOsPath pathStr
    let fileInfo = FileInfo { fileModifiedTime = modTime, fileSize = size }
    pure $ Map.insert path fileInfo acc
    ) Map.empty results

  pure $ LibrarySnapshot fileMap

-- * Metadata operations

-- | Insert metadata for a track.
insertTrackMetadata :: SQLite.Connection -> Int64 -> LibraryTrackMetadataRecord -> IO ()
insertTrackMetadata conn tid meta =
  executeQuery conn
    "INSERT INTO library_track_metadata (track_id, title, artist, album, album_artist, track_number, disc_number, year, duration_seconds, genre) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( tid
    , metaTitle meta
    , metaArtist meta
    , metaAlbum meta
    , metaAlbumArtist meta
    , metaTrackNumber meta
    , metaDiscNumber meta
    , metaYear meta
    , metaDurationSeconds meta
    , metaGenre meta
    )

-- | Update metadata for a track.
updateTrackMetadata :: SQLite.Connection -> Int64 -> LibraryTrackMetadataRecord -> IO ()
updateTrackMetadata conn tid meta =
  executeQuery conn
    "UPDATE library_track_metadata SET title = ?, artist = ?, album = ?, album_artist = ?, \
    \track_number = ?, disc_number = ?, year = ?, duration_seconds = ?, genre = ?, \
    \updated_at = CURRENT_TIMESTAMP WHERE track_id = ?"
    ( metaTitle meta
    , metaArtist meta
    , metaAlbum meta
    , metaAlbumArtist meta
    , metaTrackNumber meta
    , metaDiscNumber meta
    , metaYear meta
    , metaDurationSeconds meta
    , metaGenre meta
    , tid
    )

-- | Get metadata for a track.
getMetadataForTrack :: SQLite.Connection -> Int64 -> IO (Maybe LibraryTrackMetadataRecord)
getMetadataForTrack conn tid = do
  results <- queryRows conn
    "SELECT track_id, format, title, artist, album, album_artist, track_number, total_tracks, disc_number, total_discs, date, year, genre, publisher, comment, duration_seconds, bits_per_sample, country, label, catalog_number, barcode, release_status, release_type, mb_recording_id, mb_track_id, mb_release_id, mb_release_group_id, mb_artist_id, mb_album_artist_id, mb_work_id, mb_disc_id, acoustid_fingerprint, acoustid_id, created_at, updated_at \
    \FROM library_track_metadata WHERE track_id = ?"
    (Only tid)
  pure $ viaNonEmpty head results

-- | Insert or update track with metadata (upsert).
-- Returns the track ID.
-- Optimized to use fewer queries.
upsertTrackWithMetadata :: SQLite.Connection -> OsPath -> Metadata -> IO Int64
upsertTrackWithMetadata conn path meta = do
  pathStr <- OP.decodeUtf path
  now <- getCurrentTime

  -- Get file size and modification time from filesystem
  fSize <- getFileSize path
  modTime <- getModificationTime path

  -- Compute quality from audio metadata
  let quality = detectQualityFromAudio meta
      qualityText = qualityToText quality

  -- Upsert track record using INSERT OR REPLACE (SQLite)
  -- This handles both insert and update in one query
  executeQuery conn
    "INSERT INTO library_tracks (path, size, modified_at, added_at, updated_at, quality) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(path) DO UPDATE SET \
    \  size = excluded.size, \
    \  modified_at = excluded.modified_at, \
    \  quality = excluded.quality, \
    \  updated_at = CURRENT_TIMESTAMP"
    (pathStr :: String, fSize, modTime, now, now, qualityText)

  -- Get track ID
  results <- queryRows conn
    "SELECT id FROM library_tracks WHERE path = ?"
    (Only (pathStr :: String)) :: IO [Only Int64]

  tid <- case viaNonEmpty head results of
    Just (Only tid) -> pure tid
    Nothing -> fail "Failed to get track ID after upsert"  -- Should never happen with UPSERT

  -- Upsert metadata with MB IDs using custom ToRow instance
  let mbIds = M.musicBrainzIds meta
      audioProps = M.audioProperties meta
  let metadataInsert = MetadataInsert
        { miTrackId = tid
        , miTitle = M.title meta
        , miArtist = M.artist meta
        , miAlbum = M.album meta
        , miAlbumArtist = M.albumArtist meta
        , miTrackNumber = M.trackNumber meta
        , miDiscNumber = M.discNumber meta
        , miYear = M.year meta
        , miDurationSeconds = fmap (\ms -> (fromIntegral ms :: Double) / 1000.0) (M.duration audioProps)
        , miGenre = M.genre meta
        , miDate = M.date meta
        , miBarcode = M.barcode meta
        , miCatalogNumber = M.catalogNumber meta
        , miLabel = M.recordLabel meta
        , miCountry = M.releaseCountry meta
        , miMBRecordingId = M.mbRecordingId mbIds
        , miMBTrackId = M.mbTrackId mbIds
        , miMBReleaseId = M.mbReleaseId mbIds
        , miMBReleaseGroupId = M.mbReleaseGroupId mbIds
        , miMBArtistId = M.mbArtistId mbIds
        , miFormat = Just (audioFormatToText $ M.format meta)
        , miBitrate = M.bitrate audioProps
        , miSampleRate = M.sampleRate audioProps
        , miBitsPerSample = M.bitsPerSample audioProps
        }

  executeQuery conn
    "INSERT INTO library_track_metadata (track_id, title, artist, album, album_artist, track_number, disc_number, year, duration_seconds, genre, date, barcode, catalog_number, label, country, mb_recording_id, mb_track_id, mb_release_id, mb_release_group_id, mb_artist_id, format, bitrate, sample_rate, bits_per_sample) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(track_id) DO UPDATE SET \
    \  title = excluded.title, \
    \  artist = excluded.artist, \
    \  album = excluded.album, \
    \  album_artist = excluded.album_artist, \
    \  track_number = excluded.track_number, \
    \  disc_number = excluded.disc_number, \
    \  year = excluded.year, \
    \  duration_seconds = excluded.duration_seconds, \
    \  genre = excluded.genre, \
    \  date = excluded.date, \
    \  barcode = excluded.barcode, \
    \  catalog_number = excluded.catalog_number, \
    \  label = excluded.label, \
    \  country = excluded.country, \
    \  mb_recording_id = excluded.mb_recording_id, \
    \  mb_track_id = excluded.mb_track_id, \
    \  mb_release_id = excluded.mb_release_id, \
    \  mb_release_group_id = excluded.mb_release_group_id, \
    \  mb_artist_id = excluded.mb_artist_id, \
    \  format = excluded.format, \
    \  bitrate = excluded.bitrate, \
    \  sample_rate = excluded.sample_rate, \
    \  bits_per_sample = excluded.bits_per_sample, \
    \  updated_at = CURRENT_TIMESTAMP"
    metadataInsert

  pure tid

-- * MusicBrainz operations

-- | Update MusicBrainz IDs for a track.
-- Note: MB IDs are now stored on library_tracks table, not library_track_metadata.
updateMusicBrainzIds :: SQLite.Connection
                     -> OsPath
                     -> Maybe Text  -- Recording ID
                     -> Maybe Text  -- Track ID
                     -> Maybe Text  -- Release ID
                     -> Maybe Text  -- Release Group ID
                     -> Maybe Text  -- Artist ID
                     -> Maybe Double  -- Confidence
                     -> IO ()
updateMusicBrainzIds conn path mbRecordingId mbTrackId mbReleaseId mbReleaseGroupId mbArtistId confidence = do
  pathStr <- OP.decodeUtf path
  executeQuery conn
    "UPDATE library_tracks SET \
    \mb_recording_id = ?, mb_track_id = ?, mb_release_id = ?, \
    \mb_release_group_id = ?, mb_artist_id = ?, mb_confidence = ?, \
    \updated_at = CURRENT_TIMESTAMP \
    \WHERE path = ?"
    ( mbRecordingId
    , mbTrackId
    , mbReleaseId
    , mbReleaseGroupId
    , mbArtistId
    , confidence
    , pathStr :: String
    )

-- | Get MusicBrainz IDs for a track by path.
-- Note: MB IDs are now stored on library_tracks table, not library_track_metadata.
getMusicBrainzIdsByPath :: SQLite.Connection -> OsPath -> IO (Maybe (Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Double))
getMusicBrainzIdsByPath conn path = do
  pathStr <- OP.decodeUtf path
  results <- queryRows conn
    "SELECT mb_recording_id, mb_track_id, mb_release_id, mb_release_group_id, mb_artist_id, mb_confidence \
    \FROM library_tracks \
    \WHERE path = ?"
    (Only (pathStr :: String))
  pure $ viaNonEmpty head results

-- * Scan history

-- | Insert a new scan history record.
insertScanHistory :: SQLite.Connection -> UTCTime -> Bool -> IO (Maybe Int64)
insertScanHistory conn startTime libAvailable = do
  executeQuery conn
    "INSERT INTO scan_history (started_at, library_available) VALUES (?, ?)"
    (startTime, libAvailable)
  -- TODO: Return the inserted ID
  pure Nothing

-- | Update scan history with results.
updateScanHistory :: SQLite.Connection -> Int64 -> UTCTime -> Int -> Int -> Int -> Maybe Text -> IO ()
updateScanHistory conn sid completedAt added modified deleted maybeError =
  executeQuery conn
    "UPDATE scan_history SET completed_at = ?, files_added = ?, files_modified = ?, files_deleted = ?, error = ? WHERE id = ?"
    (completedAt, added, modified, deleted, maybeError, sid)

-- | Get recent scan history.
getRecentScans :: SQLite.Connection -> Int -> IO [ScanHistoryRecord]
getRecentScans conn limit =
  queryRows conn
    "SELECT id, started_at, completed_at, files_added, files_modified, files_deleted, library_available, error \
    \FROM scan_history ORDER BY started_at DESC LIMIT ?"
    (Only limit)
