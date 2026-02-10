{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database migrations.
--
-- Creates the database schema on first run.
module Skema.Database.Migrations
  ( runMigrations
  ) where

import Skema.Database.Connection
import Skema.Database.Types (sourceTypeToText, SourceType(..))
import Katip
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import qualified Data.Text.ICU.Normalize as ICU

-- | Run all pending migrations.
runMigrations :: LogEnv -> ConnectionPool -> IO ()
runMigrations le connPool = do
  let initialContext = ()
  let initialNamespace = "database"

  runKatipContextT le initialContext initialNamespace $ do
    liftIO $ withConnection connPool $ \conn -> do
      -- Create schema version tracking table
      createMigrationsTable conn

      -- Create schema
      createSchema conn

      -- Run incremental migrations
      runIncrementalMigrations le conn

      -- Create default data
      createDefaultAcquisitionSource conn
      createBestNewMusicSources conn
      createDefaultQualityProfiles conn
      initializeSettings conn

    $(logTM) InfoS $ logStr ("Database migrations completed successfully" :: Text)

-- | Create migrations tracking table
createMigrationsTable :: SQLite.Connection -> IO ()
createMigrationsTable conn =
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS schema_migrations ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  migration_name TEXT NOT NULL UNIQUE, \
    \  applied_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

-- | Check if a migration has been applied
migrationApplied :: SQLite.Connection -> Text -> IO Bool
migrationApplied conn name = do
  results <- queryRows conn
    "SELECT COUNT(*) FROM schema_migrations WHERE migration_name = ?"
    (Only name) :: IO [Only Int]
  pure $ case viaNonEmpty head results of
    Just (Only count) -> count > 0
    Nothing -> False

-- | Check if a column exists in a table
columnExists :: SQLite.Connection -> Text -> Text -> IO Bool
columnExists conn tableName columnName = do
  -- Use PRAGMA table_info to check if column exists
  results <- queryRows conn
    ("PRAGMA table_info(" <> tableName <> ")")
    () :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
  pure $ any (\(_, name, _, _, _, _) -> name == columnName) results

-- | Check if a table exists
tableExists :: SQLite.Connection -> Text -> IO Bool
tableExists conn tableName = do
  results <- queryRows conn
    "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
    (Only tableName) :: IO [Only Text]
  pure $ not (null results)

-- | Record that a migration has been applied
recordMigration :: SQLite.Connection -> Text -> IO ()
recordMigration conn name =
  executeQuery conn
    "INSERT INTO schema_migrations (migration_name) VALUES (?)"
    (Only name)

-- | Run incremental migrations
runIncrementalMigrations :: LogEnv -> SQLite.Connection -> IO ()
runIncrementalMigrations le conn = do
  let initialContext = ()
  let initialNamespace = "database.migrations"

  runKatipContextT le initialContext initialNamespace $ do
    -- Migration 001: Track recording matching and match provenance (squashed from 001-004)
    applied <- liftIO $ migrationApplied conn "001_track_recording_and_provenance"
    unless applied $ do
      $(logTM) InfoS "Running migration: 001_track_recording_and_provenance"
      liftIO $ do
        -- Add match provenance columns to clusters
        -- Check if columns exist before adding them to avoid errors
        matchSourceExists <- columnExists conn "clusters" "match_source"
        unless matchSourceExists $
          executeQuery_ conn "ALTER TABLE clusters ADD COLUMN match_source TEXT"

        matchLockedExists <- columnExists conn "clusters" "match_locked"
        unless matchLockedExists $
          executeQuery_ conn "ALTER TABLE clusters ADD COLUMN match_locked INTEGER NOT NULL DEFAULT 0"

        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_clusters_match_locked ON clusters(match_locked)"

        -- Add track recording title column (mb_recording_id already exists)
        recordingTitleExists <- columnExists conn "library_tracks" "mb_recording_title"
        unless recordingTitleExists $
          executeQuery_ conn "ALTER TABLE library_tracks ADD COLUMN mb_recording_title TEXT"

        -- Create cluster match candidates table
        executeQuery_ conn
          "CREATE TABLE IF NOT EXISTS cluster_match_candidates ( \
          \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
          \  cluster_id INTEGER NOT NULL, \
          \  mb_release_id TEXT NOT NULL, \
          \  confidence REAL NOT NULL, \
          \  match_data TEXT, \
          \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
          \  FOREIGN KEY (cluster_id) REFERENCES clusters(id) ON DELETE CASCADE \
          \)"
        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_candidates_cluster ON cluster_match_candidates(cluster_id)"
        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_candidates_confidence ON cluster_match_candidates(cluster_id, confidence DESC)"

        -- Populate track recording IDs from release data
        executeQuery_ conn
          "UPDATE library_tracks SET \
          \  mb_recording_id = (\
          \    SELECT json_extract(track.value, '$.recording.id') \
          \    FROM clusters c, json_each(json_extract(c.mb_release_data, '$.media')) AS media, \
          \         json_each(json_extract(media.value, '$.tracks')) AS track \
          \    WHERE c.id = library_tracks.cluster_id \
          \      AND json_extract(track.value, '$.position') = (\
          \        SELECT track_number FROM library_track_metadata WHERE track_id = library_tracks.id\
          \      )\
          \  ), \
          \  mb_recording_title = (\
          \    SELECT json_extract(track.value, '$.title') \
          \    FROM clusters c, json_each(json_extract(c.mb_release_data, '$.media')) AS media, \
          \         json_each(json_extract(media.value, '$.tracks')) AS track \
          \    WHERE c.id = library_tracks.cluster_id \
          \      AND json_extract(track.value, '$.position') = (\
          \        SELECT track_number FROM library_track_metadata WHERE track_id = library_tracks.id\
          \      )\
          \  ) \
          \WHERE cluster_id IS NOT NULL \
          \  AND mb_recording_id IS NULL \
          \  AND EXISTS (SELECT 1 FROM clusters WHERE id = library_tracks.cluster_id AND mb_release_data IS NOT NULL)"

    -- Migration 002: Quality profiles and catalog improvements
    applied002 <- liftIO $ migrationApplied conn "002_quality_profiles_and_catalog"
    unless applied002 $ do
      $(logTM) InfoS "Running migration: 002_quality_profiles_and_catalog"
      liftIO $ do
        -- Add RSS state tracking
        executeQuery_ conn
          "CREATE TABLE IF NOT EXISTS indexer_rss_state ( \
          \  url TEXT PRIMARY KEY, \
          \  name TEXT NOT NULL, \
          \  last_seen_guid TEXT, \
          \  last_check_at TIMESTAMP, \
          \  last_successful_check_at TIMESTAMP, \
          \  supports_rss_pagination INTEGER, \
          \  capabilities_detected_at TIMESTAMP, \
          \  consecutive_failures INTEGER NOT NULL DEFAULT 0, \
          \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
          \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
          \)"

        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_indexer_rss_state_last_check ON indexer_rss_state(last_check_at)"

        -- Add RSS monitoring settings to settings table
        rssEnabledExists <- columnExists conn "settings" "rss_sync_enabled"
        unless rssEnabledExists $
          executeQuery_ conn "ALTER TABLE settings ADD COLUMN rss_sync_enabled INTEGER NOT NULL DEFAULT 1"

        rssIntervalExists <- columnExists conn "settings" "rss_sync_interval_seconds"
        unless rssIntervalExists $
          executeQuery_ conn "ALTER TABLE settings ADD COLUMN rss_sync_interval_seconds INTEGER NOT NULL DEFAULT 900"

        rssThresholdExists <- columnExists conn "settings" "rss_sync_max_threshold_hours"
        unless rssThresholdExists $
          executeQuery_ conn "ALTER TABLE settings ADD COLUMN rss_sync_max_threshold_hours INTEGER NOT NULL DEFAULT 72"

        -- Add timestamps to catalog_albums (from migration 003)
        catalogCreatedAtExists <- columnExists conn "catalog_albums" "created_at"
        unless catalogCreatedAtExists $
          executeQuery_ conn "ALTER TABLE catalog_albums ADD COLUMN created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP"

        catalogUpdatedAtExists <- columnExists conn "catalog_albums" "updated_at"
        unless catalogUpdatedAtExists $
          executeQuery_ conn "ALTER TABLE catalog_albums ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP"

        -- Add audio quality tracking (from migration 004)
        bitrateExists <- columnExists conn "library_track_metadata" "bitrate"
        unless bitrateExists $
          executeQuery_ conn "ALTER TABLE library_track_metadata ADD COLUMN bitrate INTEGER"

        sampleRateExists <- columnExists conn "library_track_metadata" "sample_rate"
        unless sampleRateExists $
          executeQuery_ conn "ALTER TABLE library_track_metadata ADD COLUMN sample_rate INTEGER"

        qualityExists <- columnExists conn "library_tracks" "quality"
        unless qualityExists $ do
          executeQuery_ conn "ALTER TABLE library_tracks ADD COLUMN quality TEXT"
          -- Reset modification times to force a full rescan and populate quality data
          executeQuery_ conn "UPDATE library_tracks SET modified_at = '1970-01-01 00:00:00'"

        -- Update catalog album quality from cluster track quality
        -- Note: This uses matched_cluster_id if it exists (during migration),
        -- but after migration 004, this column won't exist anymore
        matchedClusterIdExists <- columnExists conn "catalog_albums" "matched_cluster_id"
        when matchedClusterIdExists $
          executeQuery_ conn
            "UPDATE catalog_albums \
            \SET current_quality = ( \
            \  SELECT MIN(quality) \
            \  FROM library_tracks \
            \  WHERE cluster_id = catalog_albums.matched_cluster_id \
            \  AND quality IS NOT NULL \
            \) \
            \WHERE matched_cluster_id IS NOT NULL"

        -- Remove wanted and user_unwanted columns (derived state, not stored)
        -- The "wanted" status is now computed from quality_profile_id + current_quality + matched_cluster_id
        wantedExists <- columnExists conn "catalog_albums" "wanted"
        when wantedExists $ do
          -- Get default quality profile ID from settings
          defaultProfileResults <- queryRows conn
            "SELECT default_quality_profile_id FROM settings WHERE id = 1"
            () :: IO [Only (Maybe Int)]
          let defaultProfileId = case viaNonEmpty head defaultProfileResults of
                Just (Only (Just pid)) -> pid
                _ -> 2  -- Fallback to "Lossless Preferred" profile ID

          -- SQLite doesn't support DROP COLUMN directly, need to recreate table
          executeQuery_ conn
            "CREATE TABLE catalog_albums_new ( \
            \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \  release_group_mbid TEXT NOT NULL UNIQUE, \
            \  title TEXT NOT NULL, \
            \  artist_id INTEGER REFERENCES catalog_artists(id) ON DELETE CASCADE, \
            \  artist_mbid TEXT NOT NULL, \
            \  artist_name TEXT NOT NULL, \
            \  album_type TEXT, \
            \  first_release_date TEXT, \
            \  album_cover_url TEXT, \
            \  album_cover_thumbnail_url TEXT, \
            \  matched_cluster_id INTEGER REFERENCES clusters(id) ON DELETE SET NULL, \
            \  quality_profile_id INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL, \
            \  current_quality TEXT, \
            \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
            \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
            \)"

          -- Copy data from old table, preserving wanted status by assigning default quality profile
          -- If album was wanted=1 but has no quality_profile_id, assign the default profile
          executeQuery conn
            "INSERT INTO catalog_albums_new \
            \  (id, release_group_mbid, title, artist_id, artist_mbid, artist_name, \
            \   album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, \
            \   matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at) \
            \SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, \
            \       album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, \
            \       matched_cluster_id, \
            \       CASE \
            \         WHEN quality_profile_id IS NOT NULL THEN quality_profile_id \
            \         WHEN wanted = 1 THEN ? \
            \         ELSE NULL \
            \       END, \
            \       current_quality, \
            \       COALESCE(created_at, CURRENT_TIMESTAMP), COALESCE(updated_at, CURRENT_TIMESTAMP) \
            \FROM catalog_albums"
            (Only defaultProfileId)

          -- Drop old table and rename new one
          executeQuery_ conn "DROP TABLE catalog_albums"
          executeQuery_ conn "ALTER TABLE catalog_albums_new RENAME TO catalog_albums"

          -- Recreate indexes
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_release_group_mbid ON catalog_albums(release_group_mbid)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_id ON catalog_albums(artist_id)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_mbid ON catalog_albums(artist_mbid)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_matched_cluster_id ON catalog_albums(matched_cluster_id)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_created_at ON catalog_albums(created_at DESC)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_quality_profile_id ON catalog_albums(quality_profile_id)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_current_quality ON catalog_albums(current_quality)"

        recordMigration conn "002_quality_profiles_and_catalog"
      $(logTM) InfoS "Completed migration: 002_quality_profiles_and_catalog"

    -- Migration 003: Remove legacy wanted_albums and tracked_artists tables
    applied003 <- liftIO $ migrationApplied conn "003_remove_legacy_wanted_tracked_tables"
    unless applied003 $ do
      $(logTM) InfoS "Running migration: 003_remove_legacy_wanted_tracked_tables"
      liftIO $ do
        -- Drop wanted_albums table if it exists
        wantedExists <- tableExists conn "wanted_albums"
        when wantedExists $ do
          executeQuery_ conn "DROP TABLE wanted_albums"

        -- Drop tracked_artists table if it exists
        trackedExists <- tableExists conn "tracked_artists"
        when trackedExists $ do
          executeQuery_ conn "DROP TABLE tracked_artists"

        recordMigration conn "003_remove_legacy_wanted_tracked_tables"
      $(logTM) InfoS "Completed migration: 003_remove_legacy_wanted_tracked_tables"

    -- Migration 004: Drop matched_cluster_id column from catalog_albums
    applied004 <- liftIO $ migrationApplied conn "004_drop_catalog_albums_matched_cluster_id"
    unless applied004 $ do
      $(logTM) InfoS "Running migration: 004_drop_catalog_albums_matched_cluster_id"
      liftIO $ do
        -- Check if column still exists before attempting to drop
        matchedClusterExists <- columnExists conn "catalog_albums" "matched_cluster_id"
        when matchedClusterExists $ do
          -- SQLite doesn't support DROP COLUMN directly, need to recreate table
          executeQuery_ conn
            "CREATE TABLE catalog_albums_new ( \
            \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
            \  release_group_mbid TEXT NOT NULL UNIQUE, \
            \  title TEXT NOT NULL, \
            \  artist_id INTEGER REFERENCES catalog_artists(id) ON DELETE CASCADE, \
            \  artist_mbid TEXT NOT NULL, \
            \  artist_name TEXT NOT NULL, \
            \  album_type TEXT, \
            \  first_release_date TEXT, \
            \  album_cover_url TEXT, \
            \  album_cover_thumbnail_url TEXT, \
            \  quality_profile_id INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL, \
            \  current_quality TEXT, \
            \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
            \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
            \)"

          -- Copy data from old table (excluding matched_cluster_id)
          executeQuery_ conn
            "INSERT INTO catalog_albums_new \
            \  (id, release_group_mbid, title, artist_id, artist_mbid, artist_name, \
            \   album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, \
            \   quality_profile_id, current_quality, created_at, updated_at) \
            \SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, \
            \       album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, \
            \       quality_profile_id, current_quality, created_at, updated_at \
            \FROM catalog_albums"

          -- Drop old table and rename new one
          executeQuery_ conn "DROP TABLE catalog_albums"
          executeQuery_ conn "ALTER TABLE catalog_albums_new RENAME TO catalog_albums"

          -- Recreate indexes (excluding matched_cluster_id index)
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_release_group_mbid ON catalog_albums(release_group_mbid)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_id ON catalog_albums(artist_id)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_mbid ON catalog_albums(artist_mbid)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_created_at ON catalog_albums(created_at DESC)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_quality_profile_id ON catalog_albums(quality_profile_id)"
          executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_current_quality ON catalog_albums(current_quality)"

        recordMigration conn "004_drop_catalog_albums_matched_cluster_id"
      $(logTM) InfoS "Completed migration: 004_drop_catalog_albums_matched_cluster_id"

    -- Migration 005: Add quality column to clusters table
    applied005 <- liftIO $ migrationApplied conn "005_add_quality_to_clusters"
    unless applied005 $ do
      $(logTM) InfoS "Running migration: 005_add_quality_to_clusters"
      liftIO $ do
        -- Check if column already exists
        qualityExists <- columnExists conn "clusters" "quality"
        unless qualityExists $ do
          -- Add quality column to clusters table
          executeQuery_ conn
            "ALTER TABLE clusters ADD COLUMN quality TEXT"

          -- Create index on quality for faster queries
          executeQuery_ conn
            "CREATE INDEX IF NOT EXISTS idx_clusters_quality ON clusters(quality)"

          -- Backfill quality for existing clusters by computing from their tracks
          executeQuery_ conn
            "UPDATE clusters \
            \SET quality = ( \
            \  SELECT \
            \    CASE MIN( \
            \      CASE quality \
            \        WHEN 'unknown' THEN 1 \
            \        WHEN 'mp3_192' THEN 2 \
            \        WHEN 'vbr2' THEN 3 \
            \        WHEN 'mp3_256' THEN 4 \
            \        WHEN 'vbr0' THEN 5 \
            \        WHEN 'mp3_320' THEN 6 \
            \        WHEN 'lossless' THEN 7 \
            \        WHEN 'hires_lossless' THEN 8 \
            \        ELSE 0 \
            \      END \
            \    ) \
            \      WHEN 1 THEN 'unknown' \
            \      WHEN 2 THEN 'mp3_192' \
            \      WHEN 3 THEN 'vbr2' \
            \      WHEN 4 THEN 'mp3_256' \
            \      WHEN 5 THEN 'vbr0' \
            \      WHEN 6 THEN 'mp3_320' \
            \      WHEN 7 THEN 'lossless' \
            \      WHEN 8 THEN 'hires_lossless' \
            \      ELSE NULL \
            \    END \
            \  FROM library_tracks \
            \  WHERE library_tracks.cluster_id = clusters.id \
            \    AND library_tracks.quality IS NOT NULL \
            \) \
            \WHERE EXISTS ( \
            \  SELECT 1 FROM library_tracks \
            \  WHERE library_tracks.cluster_id = clusters.id \
            \    AND library_tracks.quality IS NOT NULL \
            \)"

          -- Backfill catalog_albums.current_quality from cluster quality
          executeQuery_ conn
            "UPDATE catalog_albums \
            \SET current_quality = ( \
            \  SELECT c.quality \
            \  FROM clusters c \
            \  WHERE c.mb_release_group_id = catalog_albums.release_group_mbid \
            \), \
            \updated_at = CURRENT_TIMESTAMP \
            \WHERE current_quality IS NULL \
            \  AND EXISTS ( \
            \    SELECT 1 FROM clusters c \
            \    WHERE c.mb_release_group_id = catalog_albums.release_group_mbid \
            \      AND c.quality IS NOT NULL \
            \  )"

        recordMigration conn "005_add_quality_to_clusters"
      $(logTM) InfoS "Completed migration: 005_add_quality_to_clusters"

    -- Migration 006: Add normalized search columns for artists and albums
    applied006 <- liftIO $ migrationApplied conn "006_add_normalized_search_columns"
    unless applied006 $ do
      $(logTM) InfoS "Running migration: 006_add_normalized_search_columns"
      liftIO $ do
        -- Add normalized search column to catalog_artists
        artistSearchExists <- columnExists conn "catalog_artists" "artist_name_normalized"
        unless artistSearchExists $ do
          executeQuery_ conn
            "ALTER TABLE catalog_artists ADD COLUMN artist_name_normalized TEXT"

        -- Always backfill normalized names for existing artists (in case normalization changed)
        artists <- queryRows conn
          "SELECT id, artist_name FROM catalog_artists WHERE artist_name_normalized IS NULL OR artist_name_normalized = ''"
          () :: IO [(Int64, Text)]
        forM_ artists $ \(artistId, artistName) -> do
          let normalized = normalizeForSearch artistName
          executeQuery conn
            "UPDATE catalog_artists SET artist_name_normalized = ? WHERE id = ?"
            (normalized, artistId)

        -- Create index for fast searching
        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_catalog_artists_search ON catalog_artists(artist_name_normalized)"

        -- Add normalized search columns to catalog_albums
        albumTitleSearchExists <- columnExists conn "catalog_albums" "title_normalized"
        unless albumTitleSearchExists $ do
          executeQuery_ conn
            "ALTER TABLE catalog_albums ADD COLUMN title_normalized TEXT"

        albumArtistSearchExists <- columnExists conn "catalog_albums" "artist_name_normalized"
        unless albumArtistSearchExists $ do
          executeQuery_ conn
            "ALTER TABLE catalog_albums ADD COLUMN artist_name_normalized TEXT"

        -- Always backfill normalized titles for existing albums (in case normalization changed)
        albums <- queryRows conn
          "SELECT id, title, artist_name FROM catalog_albums WHERE title_normalized IS NULL OR title_normalized = '' OR artist_name_normalized IS NULL OR artist_name_normalized = ''"
          () :: IO [(Int64, Text, Text)]
        forM_ albums $ \(albumId, title, artistName) -> do
          let normalizedTitle = normalizeForSearch title
          let normalizedArtist = normalizeForSearch artistName
          executeQuery conn
            "UPDATE catalog_albums SET title_normalized = ?, artist_name_normalized = ? WHERE id = ?"
            (normalizedTitle, normalizedArtist, albumId)

        -- Create indexes for fast searching
        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_catalog_albums_title_search ON catalog_albums(title_normalized)"
        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_search ON catalog_albums(artist_name_normalized)"

        recordMigration conn "006_add_normalized_search_columns"
      $(logTM) InfoS "Completed migration: 006_add_normalized_search_columns"

    -- Migration 007: Add slskd tracking columns to downloads table
    applied007 <- liftIO $ migrationApplied conn "007_add_slskd_columns"
    unless applied007 $ do
      $(logTM) InfoS "Running migration: 007_add_slskd_columns"
      liftIO $ do
        -- Add slskd_username column to track which user the files are from
        slskdUsernameExists <- columnExists conn "downloads" "slskd_username"
        unless slskdUsernameExists $ do
          executeQuery_ conn
            "ALTER TABLE downloads ADD COLUMN slskd_username TEXT"

        -- Add slskd_files_total to track total number of files
        slskdFilesTotalExists <- columnExists conn "downloads" "slskd_files_total"
        unless slskdFilesTotalExists $ do
          executeQuery_ conn
            "ALTER TABLE downloads ADD COLUMN slskd_files_total INTEGER"

        -- Add slskd_files_completed to track completed files
        slskdFilesCompletedExists <- columnExists conn "downloads" "slskd_files_completed"
        unless slskdFilesCompletedExists $ do
          executeQuery_ conn
            "ALTER TABLE downloads ADD COLUMN slskd_files_completed INTEGER"

        -- Create index for efficient queries on slskd downloads
        executeQuery_ conn
          "CREATE INDEX IF NOT EXISTS idx_downloads_slskd_username ON downloads(slskd_username)"

        recordMigration conn "007_add_slskd_columns"
      $(logTM) InfoS "Completed migration: 007_add_slskd_columns"

-- | Normalize text for search by:
-- 1. Decomposing accented characters (NFD normalization)
-- 2. Removing diacritical marks
-- 3. Converting to lowercase
-- 4. Keeping only alphanumeric characters and spaces (removes punctuation)
-- 5. Collapsing multiple spaces into one
normalizeForSearch :: Text -> Text
normalizeForSearch text =
  let decomposed = ICU.normalize ICU.NFD text  -- Decompose accents
      lowered = T.toLower decomposed
      stripped = T.filter (\c -> isAlphaNum c || c == ' ') lowered  -- Keep alphanumeric and spaces
      collapsed = T.unwords $ T.words stripped  -- Collapse multiple spaces
  in collapsed


-- | Create the complete database schema.
createSchema :: SQLite.Connection -> IO ()
createSchema conn = do
  -- Create library_tracks table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS library_tracks ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  path TEXT NOT NULL UNIQUE, \
    \  size INTEGER NOT NULL, \
    \  modified_at TIMESTAMP NOT NULL, \
    \  cluster_id INTEGER, \
    \  mb_recording_id TEXT, \
    \  mb_track_id TEXT, \
    \  mb_release_id TEXT, \
    \  mb_confidence REAL, \
    \  added_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  FOREIGN KEY (cluster_id) REFERENCES clusters(id) ON DELETE SET NULL \
    \)"

  -- Create indexes on library_tracks
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_tracks_path ON library_tracks(path)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_tracks_modified_at ON library_tracks(modified_at)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_tracks_cluster_id ON library_tracks(cluster_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_tracks_mb_release_id ON library_tracks(mb_release_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_tracks_mb_recording_id ON library_tracks(mb_recording_id)"

  -- Create library_track_metadata table with all fields
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS library_track_metadata ( \
    \  track_id INTEGER PRIMARY KEY REFERENCES library_tracks(id) ON DELETE CASCADE, \
    \  title TEXT, \
    \  artist TEXT, \
    \  album TEXT, \
    \  album_artist TEXT, \
    \  track_number INTEGER, \
    \  disc_number INTEGER, \
    \  year INTEGER, \
    \  genre TEXT, \
    \  date TEXT, \
    \  barcode TEXT, \
    \  catalog_number TEXT, \
    \  label TEXT, \
    \  country TEXT, \
    \  duration_seconds REAL, \
    \  mb_recording_id TEXT, \
    \  mb_track_id TEXT, \
    \  mb_release_id TEXT, \
    \  mb_release_group_id TEXT, \
    \  mb_artist_id TEXT, \
    \  format TEXT, \
    \  total_tracks INTEGER, \
    \  total_discs INTEGER, \
    \  publisher TEXT, \
    \  comment TEXT, \
    \  bits_per_sample INTEGER, \
    \  release_status TEXT, \
    \  release_type TEXT, \
    \  mb_album_artist_id TEXT, \
    \  mb_work_id TEXT, \
    \  mb_disc_id TEXT, \
    \  acoustid_fingerprint TEXT, \
    \  acoustid_id TEXT, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_track_metadata_album ON library_track_metadata(album)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_track_metadata_album_artist ON library_track_metadata(album_artist)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_library_track_metadata_mb_release_id ON library_track_metadata(mb_release_id)"

  -- Create scan_history table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS scan_history ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  started_at TIMESTAMP NOT NULL, \
    \  completed_at TIMESTAMP, \
    \  files_added INTEGER NOT NULL DEFAULT 0, \
    \  files_modified INTEGER NOT NULL DEFAULT 0, \
    \  files_deleted INTEGER NOT NULL DEFAULT 0, \
    \  library_available INTEGER NOT NULL DEFAULT 1, \
    \  error TEXT \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_scan_history_started_at ON scan_history(started_at DESC)"

  -- Create metadata_diffs table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS metadata_diffs ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  track_id INTEGER NOT NULL REFERENCES library_tracks(id) ON DELETE CASCADE, \
    \  field_name TEXT NOT NULL, \
    \  file_value TEXT, \
    \  mb_value TEXT, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_metadata_diffs_track_id ON metadata_diffs(track_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_metadata_diffs_field_name ON metadata_diffs(field_name)"

  -- Create clusters table with caching columns
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS clusters ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  metadata_hash TEXT NOT NULL UNIQUE, \
    \  album TEXT, \
    \  album_artist TEXT, \
    \  track_count INTEGER NOT NULL, \
    \  mb_release_id TEXT, \
    \  mb_release_group_id TEXT, \
    \  mb_confidence REAL, \
    \  mb_release_data TEXT, \
    \  mb_candidates TEXT, \
    \  match_source TEXT, \
    \  match_locked INTEGER NOT NULL DEFAULT 0, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  last_identified_at TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_clusters_metadata_hash ON clusters(metadata_hash)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_clusters_mb_release_id ON clusters(mb_release_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_clusters_match_locked ON clusters(match_locked)"

  -- Create metadata_change_history table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS metadata_change_history ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  track_id INTEGER NOT NULL REFERENCES library_tracks(id) ON DELETE CASCADE, \
    \  field_name TEXT NOT NULL, \
    \  old_value TEXT, \
    \  new_value TEXT, \
    \  applied_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  reverted_at TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_metadata_change_history_track_id ON metadata_change_history(track_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_metadata_change_history_applied_at ON metadata_change_history(applied_at DESC)"

  -- Create acquisition_rules table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS acquisition_rules ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  name TEXT NOT NULL, \
    \  description TEXT, \
    \  rule_type TEXT NOT NULL, \
    \  artist_mbid TEXT, \
    \  enabled INTEGER NOT NULL DEFAULT 1, \
    \  priority INTEGER NOT NULL DEFAULT 0, \
    \  filters TEXT, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_acquisition_rules_enabled ON acquisition_rules(enabled)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_acquisition_rules_rule_type ON acquisition_rules(rule_type)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_acquisition_rules_artist_mbid ON acquisition_rules(artist_mbid)"

  -- Create catalog_artists table with all fields
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS catalog_artists ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  artist_mbid TEXT NOT NULL UNIQUE, \
    \  artist_name TEXT NOT NULL, \
    \  artist_type TEXT, \
    \  image_url TEXT, \
    \  thumbnail_url TEXT, \
    \  followed INTEGER NOT NULL DEFAULT 0, \
    \  added_by_rule_id INTEGER REFERENCES acquisition_rules(id) ON DELETE SET NULL, \
    \  source_cluster_id INTEGER REFERENCES clusters(id) ON DELETE SET NULL, \
    \  last_checked_at TIMESTAMP, \
    \  quality_profile_id INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_artist_mbid ON catalog_artists(artist_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_followed ON catalog_artists(followed)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_added_by_rule_id ON catalog_artists(added_by_rule_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_source_cluster_id ON catalog_artists(source_cluster_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_last_checked_at ON catalog_artists(last_checked_at)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_created_at ON catalog_artists(created_at DESC)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_quality_profile_id ON catalog_artists(quality_profile_id)"

  -- Create catalog_albums table with all fields
  -- NOTE: "wanted" is NOT stored - it's computed from quality_profile_id + current_quality + cluster match
  -- NOTE: matched_cluster_id is NOT stored - it's derived via JOIN on clusters.mb_release_group_id
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS catalog_albums ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  release_group_mbid TEXT NOT NULL UNIQUE, \
    \  title TEXT NOT NULL, \
    \  artist_id INTEGER REFERENCES catalog_artists(id) ON DELETE CASCADE, \
    \  artist_mbid TEXT NOT NULL, \
    \  artist_name TEXT NOT NULL, \
    \  album_type TEXT, \
    \  first_release_date TEXT, \
    \  album_cover_url TEXT, \
    \  album_cover_thumbnail_url TEXT, \
    \  quality_profile_id INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL, \
    \  current_quality TEXT, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_release_group_mbid ON catalog_albums(release_group_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_id ON catalog_albums(artist_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_mbid ON catalog_albums(artist_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_created_at ON catalog_albums(created_at DESC)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_quality_profile_id ON catalog_albums(quality_profile_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_current_quality ON catalog_albums(current_quality)"

  -- Create downloads table with all fields
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS downloads ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  catalog_album_id INTEGER NOT NULL REFERENCES catalog_albums(id) ON DELETE CASCADE, \
    \  indexer_name TEXT NOT NULL, \
    \  download_url TEXT NOT NULL, \
    \  download_client TEXT, \
    \  download_client_id TEXT, \
    \  status TEXT NOT NULL DEFAULT 'queued', \
    \  download_path TEXT, \
    \  library_path TEXT, \
    \  matched_cluster_id INTEGER REFERENCES clusters(id) ON DELETE SET NULL, \
    \  title TEXT NOT NULL, \
    \  size_bytes INTEGER, \
    \  quality TEXT, \
    \  format TEXT, \
    \  seeders INTEGER, \
    \  progress REAL DEFAULT 0.0, \
    \  error_message TEXT, \
    \  queued_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  started_at TIMESTAMP, \
    \  completed_at TIMESTAMP, \
    \  imported_at TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_downloads_catalog_album_id ON downloads(catalog_album_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_downloads_status ON downloads(status)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_downloads_download_client_id ON downloads(download_client_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_downloads_queued_at ON downloads(queued_at DESC)"

  -- Create quality_profiles table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS quality_profiles ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  name TEXT NOT NULL UNIQUE, \
    \  cutoff_quality TEXT NOT NULL, \
    \  quality_preferences TEXT NOT NULL, \
    \  upgrade_automatically INTEGER NOT NULL DEFAULT 1, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_quality_profiles_name ON quality_profiles(name)"

  -- Create settings table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS settings ( \
    \  id INTEGER PRIMARY KEY CHECK (id = 1), \
    \  default_quality_profile_id INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

-- | Create the default acquisition source if it doesn't exist.
createDefaultAcquisitionSource :: SQLite.Connection -> IO ()
createDefaultAcquisitionSource conn = do
  existingSources <- queryRows conn
    "SELECT COUNT(*) FROM acquisition_rules WHERE rule_type = ?"
    (Only (sourceTypeToText LibraryArtists)) :: IO [Only Int]

  case viaNonEmpty head existingSources of
    Just (Only count) | count == 0 -> do
      let defaultFilters = "{\"release_status\":\"upcoming\"}" :: Text
      executeQuery conn
        "INSERT INTO acquisition_rules (name, description, rule_type, enabled, priority, filters) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        ( "Library Artists" :: Text
        , Just ("Track all upcoming albums from artists in your library" :: Text)
        , sourceTypeToText LibraryArtists
        , True
        , 100 :: Int
        , Just defaultFilters
        )
    _ -> pure ()

-- | Create "Best New Music" acquisition sources if they don't exist.
createBestNewMusicSources :: SQLite.Connection -> IO ()
createBestNewMusicSources conn = do
  -- Pitchfork Best New Music
  existingPitchfork <- queryRows conn
    "SELECT COUNT(*) FROM acquisition_rules WHERE name = ?"
    (Only ("Best New Music (Pitchfork)" :: Text)) :: IO [Only Int]

  case viaNonEmpty head existingPitchfork of
    Just (Only count) | count == 0 -> do
      let pitchforkFilters = "{\"genres\":[\"pop\",\"rock\",\"experimental\",\"electronic\",\"rap\",\"jazz\",\"metal\",\"folkcountry\"],\"min_score\":9.0}" :: Text
      executeQuery conn
        "INSERT INTO acquisition_rules (name, description, rule_type, enabled, priority, filters) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        ( "Best New Music (Pitchfork)" :: Text
        , Just ("Track highly acclaimed albums from Pitchfork (9.0+ score, all genres)" :: Text)
        , sourceTypeToText Pitchfork
        , False
        , 50 :: Int
        , Just pitchforkFilters
        )
    _ -> pure ()

  -- Metacritic Best New Music
  existingMetacritic <- queryRows conn
    "SELECT COUNT(*) FROM acquisition_rules WHERE name = ?"
    (Only ("Best New Music (Metacritic)" :: Text)) :: IO [Only Int]

  case viaNonEmpty head existingMetacritic of
    Just (Only count) | count == 0 -> do
      let metacriticFilters = "{\"genres\":[\"pop\",\"rock\",\"alternative\",\"rap\",\"country\",\"electronic\",\"r&b\",\"jazz\",\"folk\",\"metal\"],\"min_critic_score\":90}" :: Text
      executeQuery conn
        "INSERT INTO acquisition_rules (name, description, rule_type, enabled, priority, filters) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        ( "Best New Music (Metacritic)" :: Text
        , Just ("Track highly acclaimed albums from Metacritic (90+ score, all genres)" :: Text)
        , sourceTypeToText Metacritic
        , False
        , 50 :: Int
        , Just metacriticFilters
        )
    _ -> pure ()

-- | Create default quality profiles if they don't exist.
createDefaultQualityProfiles :: SQLite.Connection -> IO ()
createDefaultQualityProfiles conn = do
  existingProfiles <- queryRows conn
    "SELECT COUNT(*) FROM quality_profiles"
    () :: IO [Only Int]

  case viaNonEmpty head existingProfiles of
    Just (Only count) | count == 0 -> do
      -- 1. "Any Quality" - accept everything, no upgrades
      executeQuery conn
        "INSERT INTO quality_profiles (name, cutoff_quality, quality_preferences, upgrade_automatically) \
        \VALUES (?, ?, ?, ?)"
        ( "Any Quality" :: Text
        , "unknown" :: Text
        , "[{\"quality\":\"unknown\",\"rank\":100,\"enabled\":true},{\"quality\":\"mp3_192\",\"rank\":100,\"enabled\":true},{\"quality\":\"vbr2\",\"rank\":100,\"enabled\":true},{\"quality\":\"mp3_256\",\"rank\":100,\"enabled\":true},{\"quality\":\"vbr0\",\"rank\":100,\"enabled\":true},{\"quality\":\"mp3_320\",\"rank\":100,\"enabled\":true},{\"quality\":\"lossless\",\"rank\":100,\"enabled\":true},{\"quality\":\"hires_lossless\",\"rank\":100,\"enabled\":true}]" :: Text
        , False :: Bool
        )

      -- 2. "Lossless Preferred" - want FLAC, accept 320k/V0 temporarily
      executeQuery conn
        "INSERT INTO quality_profiles (name, cutoff_quality, quality_preferences, upgrade_automatically) \
        \VALUES (?, ?, ?, ?)"
        ( "Lossless Preferred" :: Text
        , "lossless" :: Text
        , "[{\"quality\":\"hires_lossless\",\"rank\":100,\"enabled\":false},{\"quality\":\"lossless\",\"rank\":100,\"enabled\":true},{\"quality\":\"mp3_320\",\"rank\":60,\"enabled\":true},{\"quality\":\"vbr0\",\"rank\":50,\"enabled\":true},{\"quality\":\"mp3_256\",\"rank\":0,\"enabled\":false},{\"quality\":\"vbr2\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_192\",\"rank\":0,\"enabled\":false},{\"quality\":\"unknown\",\"rank\":0,\"enabled\":false}]" :: Text
        , True :: Bool
        )

      -- 3. "Lossless Only" - FLAC/Hi-Res or nothing
      executeQuery conn
        "INSERT INTO quality_profiles (name, cutoff_quality, quality_preferences, upgrade_automatically) \
        \VALUES (?, ?, ?, ?)"
        ( "Lossless Only" :: Text
        , "lossless" :: Text
        , "[{\"quality\":\"hires_lossless\",\"rank\":100,\"enabled\":true},{\"quality\":\"lossless\",\"rank\":100,\"enabled\":true},{\"quality\":\"mp3_320\",\"rank\":0,\"enabled\":false},{\"quality\":\"vbr0\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_256\",\"rank\":0,\"enabled\":false},{\"quality\":\"vbr2\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_192\",\"rank\":0,\"enabled\":false},{\"quality\":\"unknown\",\"rank\":0,\"enabled\":false}]" :: Text
        , True :: Bool
        )

      -- 4. "Hi-Res Only" - 24-bit FLAC only
      executeQuery conn
        "INSERT INTO quality_profiles (name, cutoff_quality, quality_preferences, upgrade_automatically) \
        \VALUES (?, ?, ?, ?)"
        ( "Hi-Res Only" :: Text
        , "hires_lossless" :: Text
        , "[{\"quality\":\"hires_lossless\",\"rank\":100,\"enabled\":true},{\"quality\":\"lossless\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_320\",\"rank\":0,\"enabled\":false},{\"quality\":\"vbr0\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_256\",\"rank\":0,\"enabled\":false},{\"quality\":\"vbr2\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_192\",\"rank\":0,\"enabled\":false},{\"quality\":\"unknown\",\"rank\":0,\"enabled\":false}]" :: Text
        , True :: Bool
        )

      -- 5. "Prefer 320k" - 320k preferred over FLAC (smaller files)
      executeQuery conn
        "INSERT INTO quality_profiles (name, cutoff_quality, quality_preferences, upgrade_automatically) \
        \VALUES (?, ?, ?, ?)"
        ( "Prefer 320k" :: Text
        , "mp3_320" :: Text
        , "[{\"quality\":\"mp3_320\",\"rank\":100,\"enabled\":true},{\"quality\":\"lossless\",\"rank\":90,\"enabled\":true},{\"quality\":\"vbr0\",\"rank\":50,\"enabled\":true},{\"quality\":\"mp3_256\",\"rank\":0,\"enabled\":false},{\"quality\":\"vbr2\",\"rank\":0,\"enabled\":false},{\"quality\":\"mp3_192\",\"rank\":0,\"enabled\":false},{\"quality\":\"hires_lossless\",\"rank\":80,\"enabled\":true},{\"quality\":\"unknown\",\"rank\":0,\"enabled\":false}]" :: Text
        , True :: Bool
        )

    _ -> pure ()

-- | Initialize settings table with default values if empty.
initializeSettings :: SQLite.Connection -> IO ()
initializeSettings conn = do
  existingSettings <- queryRows conn
    "SELECT COUNT(*) FROM settings WHERE id = 1"
    () :: IO [Only Int]

  case viaNonEmpty head existingSettings of
    Just (Only count) | count == 0 -> do
      -- Default to "Lossless Preferred" profile (ID 2)
      executeQuery conn
        "INSERT INTO settings (id, default_quality_profile_id) VALUES (1, 2)"
        ()
    _ -> pure ()
