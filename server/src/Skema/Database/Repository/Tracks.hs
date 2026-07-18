{-# LANGUAGE OverloadedStrings #-}

-- | Track and metadata repository operations.
module Skema.Database.Repository.Tracks
  ( -- * Helper types
    MetadataInsert(..)
    -- * Track operations
  , insertTrack
  , deleteTrack
  , getTrackByPath
  , getAllTracks
  , getLibrarySnapshot
    -- * Metadata operations
  , insertTrackMetadata
  , updateTrackMetadata
  , getMetadataForTrack
  , upsertTrackWithMetadata
  , setTrackClusterAssignment
  , setTrackRecording
  , setTrackRecordingByPath
    -- * Track listing (library page)
  , TracksQuery(..)
  , searchLibraryTracksPage
  , getLibraryTrackStats
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Domain.Library (LibrarySnapshot(..), FileInfo(..))
import Skema.Domain.Quality (detectQualityFromAudio, qualityToText)
import Skema.FileSystem.Utils (osPathToString, stringToOsPath)
import Monatone.Metadata (Metadata(..), AudioFormat(..))
import qualified Monatone.Metadata as M
import Control.Monad (foldM)
import System.OsPath (OsPath)
import System.Directory.OsPath (getFileSize, getModificationTime)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Only(..))
import Database.SQLite.Simple.ToField (toField)
import qualified Data.Text as T
import qualified Database.SQLite.Simple.ToRow as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Map.Strict as Map

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
-- | Delete a track record.
deleteTrack :: SQLite.Connection -> Int64 -> IO ()
deleteTrack conn tid =
  executeQuery conn "DELETE FROM library_tracks WHERE id = ?" (Only tid)

-- | Get track by path.
getTrackByPath :: SQLite.Connection -> OsPath -> IO (Maybe LibraryTrackRecord)
getTrackByPath conn path = do
  pathStr <- osPathToString path
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
  pathStr <- osPathToString path
  now <- getCurrentTime

  -- Get file size and modification time from filesystem
  fSize <- getFileSize path
  modTime <- getModificationTime path

  -- Compute quality from audio metadata
  let quality = detectQualityFromAudio meta
      qualityText = qualityToText quality

  SQLite.withImmediateTransaction conn $ do
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

-- | Assign a track to a cluster, or unassign it (Nothing).
setTrackClusterAssignment :: SQLite.Connection -> Int64 -> Maybe Int64 -> IO ()
setTrackClusterAssignment conn tid maybeClusterId =
  executeQuery conn
    "UPDATE library_tracks SET cluster_id = ? WHERE id = ?"
    (maybeClusterId, tid)

-- | Set a track's matched MusicBrainz recording by track id.
setTrackRecording :: SQLite.Connection -> Int64 -> Text -> Maybe Text -> IO ()
setTrackRecording conn tid recordingId recordingTitle =
  executeQuery conn
    "UPDATE library_tracks SET mb_recording_id = ?, mb_recording_title = ? WHERE id = ?"
    (Just recordingId, recordingTitle, tid)

-- | Set a track's matched MusicBrainz recording by file path.
setTrackRecordingByPath :: SQLite.Connection -> Text -> Text -> Maybe Text -> IO ()
setTrackRecordingByPath conn path recordingId recordingTitle =
  executeQuery conn
    "UPDATE library_tracks SET mb_recording_id = ?, mb_recording_title = ? WHERE path = ?"
    (Just recordingId, recordingTitle, path)

-- * Track listing (library page)

-- | Parameters for the library track listing.
data TracksQuery = TracksQuery
  { tqOffset :: Int
  , tqLimit :: Int
  , tqFilter :: Text     -- ^ "all", "matched", "unmatched", or "locked"
  , tqSort :: Text       -- ^ "album", "artist", "track_title", "confidence", "status"
  , tqOrder :: Text      -- ^ "asc" or "desc"
  , tqSearch :: Maybe Text
  }

-- | The track listing's WHERE clause and bound parameters.
tracksPageFilters :: TracksQuery -> (Text, [SQLite.SQLData])
tracksPageFilters query =
  let filterClause = case tqFilter query of
        "matched" -> "c.mb_release_id IS NOT NULL" :: Text
        "unmatched" -> "c.mb_release_id IS NULL"
        "locked" -> "c.match_locked = 1"
        _ -> ""
      (searchClause, searchParams) = case tqSearch query of
        Nothing -> ("" :: Text, [])
        Just q ->
          let pattern = "%" <> q <> "%" :: Text
          in ( "(m.title LIKE ? OR " <>
               "m.artist LIKE ? OR " <>
               "c.album LIKE ? OR " <>
               "c.album_artist LIKE ? OR " <>
               "t.mb_recording_title LIKE ? OR " <>
               "t.path LIKE ?)"
             , replicate 6 (toField pattern)
             )
      clauses = filter (/= "") [filterClause, searchClause]
      whereClause = if null clauses then "" else "WHERE " <> T.intercalate " AND " clauses
  in (whereClause, searchParams)

-- | The shared FROM/JOIN clause for the track listing.
tracksPageFrom :: Text
tracksPageFrom =
  "FROM library_tracks t \
  \LEFT JOIN library_track_metadata m ON t.id = m.track_id \
  \LEFT JOIN clusters c ON t.cluster_id = c.id "

-- | Fetch a page of library tracks with metadata and cluster info, plus the
-- total matching count. The column list is fixed; the caller's FromRow
-- instance must consume: t.id, t.path, m.title, m.artist, m.track_number,
-- m.disc_number, m.duration_seconds, t.mb_recording_id,
-- t.mb_recording_title, t.cluster_id, c.album, c.album_artist, m.year,
-- c.mb_release_id, release title and artist from the cached release JSON,
-- c.mb_confidence, c.match_source, c.match_locked.
searchLibraryTracksPage :: SQLite.FromRow r => SQLite.Connection -> TracksQuery -> IO (Int, [r])
searchLibraryTracksPage conn query = do
  let (whereClause, params) = tracksPageFilters query
      orderByColumn = case tqSort query of
        "album" -> "c.album" :: Text
        "artist" -> "m.artist"
        "track_title" -> "m.title"
        "confidence" -> "c.mb_confidence"
        "status" -> "CASE WHEN c.mb_release_id IS NOT NULL THEN 1 ELSE 0 END"
        _ -> "c.album"
      direction = if tqOrder query == "desc" then "DESC" else "ASC" :: Text
      orderBy = orderByColumn <> " " <> direction <> ", m.disc_number, m.track_number"

  countRows <- queryRows conn
    ("SELECT COUNT(*) " <> tracksPageFrom <> whereClause)
    params
  let total = maybe 0 (\(Only n) -> n) (viaNonEmpty head countRows)

  rows <- queryRows conn
    ("SELECT \
    \  t.id, t.path, m.title, m.artist, m.track_number, m.disc_number, m.duration_seconds, \
    \  t.mb_recording_id, t.mb_recording_title, \
    \  t.cluster_id, c.album, c.album_artist, m.year, \
    \  c.mb_release_id, \
    \  json_extract(c.mb_release_data, '$.title'), \
    \  json_extract(c.mb_release_data, '$.artist-credit[0].name'), \
    \  c.mb_confidence, c.match_source, COALESCE(c.match_locked, 0) " <>
    tracksPageFrom <> whereClause <>
    " ORDER BY " <> orderBy <>
    " LIMIT ? OFFSET ?")
    (params <> [toField (tqLimit query), toField (tqOffset query)])
  pure (total, rows)

-- | Total, matched, and locked track counts for the library page.
getLibraryTrackStats :: SQLite.Connection -> IO (Int, Int, Int)
getLibraryTrackStats conn = do
  let count sql = do
        rows <- queryRows_ conn sql
        pure $ maybe 0 (\(Only n) -> n) (viaNonEmpty head rows)
  total <- count "SELECT COUNT(*) FROM library_tracks"
  matched <- count
    "SELECT COUNT(*) FROM library_tracks t \
    \LEFT JOIN clusters c ON t.cluster_id = c.id \
    \WHERE c.mb_release_id IS NOT NULL"
  locked <- count
    "SELECT COUNT(*) FROM library_tracks t \
    \LEFT JOIN clusters c ON t.cluster_id = c.id \
    \WHERE c.match_locked = 1"
  pure (total, matched, locked)
