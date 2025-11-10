{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Database migrations.
--
-- Migrations are run automatically on application startup.
module Skema.Database.Migrations
  ( runMigrations
  ) where

import Skema.Database.Connection
import Skema.Database.Types (sourceTypeToText, SourceType(..))
import Katip
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite
import Control.Monad ()

-- | Run all pending migrations.
runMigrations :: LogEnv -> ConnectionPool -> IO ()
runMigrations le connPool = do
  let initialContext = ()
  let initialNamespace = "database"

  runKatipContextT le initialContext initialNamespace $ do
    liftIO $ withConnection connPool $ \conn -> do
      -- Run SQLite migrations
      runSQLiteMigrations conn

      -- Create default acquisition source if it doesn't exist
      createDefaultAcquisitionSource conn

      -- Create Best New Music sources if they don't exist
      createBestNewMusicSources conn

      -- Fix existing default sources that have no filters (migration)
      fixDefaultAcquisitionSourceFilters conn

    $(logTM) InfoS $ logStr ("Database migrations completed successfully" :: Text)

-- | Run SQLite-specific migrations.
runSQLiteMigrations :: SQLite.Connection -> IO ()
runSQLiteMigrations conn = do
  -- Create library_tracks table (formerly files)
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

  -- Create library_track_metadata table
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

  -- Create metadata_diffs table (formerly metadata_diff)
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

  -- Create clusters table (formerly file_groups)
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
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  last_identified_at TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_clusters_metadata_hash ON clusters(metadata_hash)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_clusters_mb_release_id ON clusters(mb_release_id)"

  -- Create metadata_change_history table to track applied changes for undo
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

  -- Create acquisition_rules table (stores acquisition sources)
  -- Note: Table named "acquisition_rules" for backward compatibility, but semantically represents "sources"
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

  -- Create tracked_artists table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS tracked_artists ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  artist_mbid TEXT NOT NULL UNIQUE, \
    \  artist_name TEXT NOT NULL, \
    \  image_url TEXT, \
    \  added_by_rule_id INTEGER NOT NULL REFERENCES acquisition_rules(id) ON DELETE CASCADE, \
    \  source_cluster_id INTEGER REFERENCES clusters(id) ON DELETE SET NULL, \
    \  last_checked_at TIMESTAMP, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_tracked_artists_artist_mbid ON tracked_artists(artist_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_tracked_artists_added_by_rule_id ON tracked_artists(added_by_rule_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_tracked_artists_source_cluster_id ON tracked_artists(source_cluster_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_tracked_artists_last_checked_at ON tracked_artists(last_checked_at)"

  -- Create wanted_albums table
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS wanted_albums ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  release_group_mbid TEXT NOT NULL UNIQUE, \
    \  title TEXT NOT NULL, \
    \  artist_mbid TEXT NOT NULL, \
    \  artist_name TEXT NOT NULL, \
    \  status TEXT NOT NULL DEFAULT 'wanted', \
    \  added_by_rule_id INTEGER NOT NULL REFERENCES acquisition_rules(id) ON DELETE CASCADE, \
    \  first_release_date TEXT, \
    \  matched_cluster_id INTEGER REFERENCES clusters(id) ON DELETE SET NULL, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_wanted_albums_release_group_mbid ON wanted_albums(release_group_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_wanted_albums_artist_mbid ON wanted_albums(artist_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_wanted_albums_status ON wanted_albums(status)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_wanted_albums_added_by_rule_id ON wanted_albums(added_by_rule_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_wanted_albums_matched_cluster_id ON wanted_albums(matched_cluster_id)"

  -- Add columns for caching MusicBrainz release data and candidates
  addColumnIfNotExists conn "clusters" "mb_release_data" "TEXT"
  addColumnIfNotExists conn "clusters" "mb_candidates" "TEXT"

  -- Create catalog_artists table (replaces tracked_artists with followed flag)
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
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_artist_mbid ON catalog_artists(artist_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_followed ON catalog_artists(followed)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_added_by_rule_id ON catalog_artists(added_by_rule_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_source_cluster_id ON catalog_artists(source_cluster_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_last_checked_at ON catalog_artists(last_checked_at)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_created_at ON catalog_artists(created_at DESC)"

  -- Create catalog_albums table (replaces wanted_albums with wanted flag)
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS catalog_albums ( \
    \  id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \  release_group_mbid TEXT NOT NULL UNIQUE, \
    \  title TEXT NOT NULL, \
    \  artist_id INTEGER NOT NULL REFERENCES catalog_artists(id) ON DELETE CASCADE, \
    \  artist_mbid TEXT NOT NULL, \
    \  artist_name TEXT NOT NULL, \
    \  album_type TEXT, \
    \  first_release_date TEXT, \
    \  album_cover_url TEXT, \
    \  wanted INTEGER NOT NULL DEFAULT 0, \
    \  matched_cluster_id INTEGER REFERENCES clusters(id) ON DELETE SET NULL, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_release_group_mbid ON catalog_albums(release_group_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_id ON catalog_albums(artist_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_artist_mbid ON catalog_albums(artist_mbid)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_wanted ON catalog_albums(wanted)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_matched_cluster_id ON catalog_albums(matched_cluster_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_created_at ON catalog_albums(created_at DESC)"

  -- Add album cover URL to catalog_albums (migration for existing databases)
  addColumnIfNotExists conn "catalog_albums" "album_cover_url" "TEXT"

  -- Add thumbnail URLs for images (migration for existing databases)
  addColumnIfNotExists conn "tracked_artists" "thumbnail_url" "TEXT"
  addColumnIfNotExists conn "catalog_albums" "album_cover_thumbnail_url" "TEXT"

  -- Add acquisition-related fields to catalog_artists (migration for existing databases)
  addColumnIfNotExists conn "catalog_artists" "thumbnail_url" "TEXT"
  addColumnIfNotExists conn "catalog_artists" "added_by_rule_id" "INTEGER REFERENCES acquisition_rules(id) ON DELETE SET NULL"
  addColumnIfNotExists conn "catalog_artists" "source_cluster_id" "INTEGER REFERENCES clusters(id) ON DELETE SET NULL"
  addColumnIfNotExists conn "catalog_artists" "last_checked_at" "TIMESTAMP"

  -- Create downloads table for tracking NZB/torrent downloads
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

  -- Add user_unwanted flag to catalog_albums to track explicitly unwanted albums
  addColumnIfNotExists conn "catalog_albums" "user_unwanted" "INTEGER NOT NULL DEFAULT 0"

  -- Add artist_id FK to catalog_albums for internal ID references (migration)
  -- Note: For existing databases, this column will be added but initially NULL for existing rows.
  -- Services should populate it when they next update albums.
  addColumnIfNotExists conn "catalog_albums" "artist_id" "INTEGER REFERENCES catalog_artists(id) ON DELETE CASCADE"

  -- Add matched_cluster_id to link downloads to the clusters they created when imported
  addColumnIfNotExists conn "downloads" "matched_cluster_id" "INTEGER REFERENCES clusters(id) ON DELETE SET NULL"

  -- Add library_path to store the destination path where files were imported to
  addColumnIfNotExists conn "downloads" "library_path" "TEXT"

  -- Create quality_profiles table for managing download quality preferences
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

  -- Create settings table (singleton for global configuration)
  executeQuery_ conn
    "CREATE TABLE IF NOT EXISTS settings ( \
    \  id INTEGER PRIMARY KEY CHECK (id = 1), \
    \  default_quality_profile_id INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL, \
    \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
    \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP \
    \)"

  -- Add quality profile relationships to catalog entities
  addColumnIfNotExists conn "catalog_artists" "quality_profile_id" "INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL"
  addColumnIfNotExists conn "catalog_albums" "quality_profile_id" "INTEGER REFERENCES quality_profiles(id) ON DELETE SET NULL"

  -- Add current quality tracking to catalog albums
  addColumnIfNotExists conn "catalog_albums" "current_quality" "TEXT"

  -- Create indexes for quality profile lookups
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_artists_quality_profile_id ON catalog_artists(quality_profile_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_quality_profile_id ON catalog_albums(quality_profile_id)"
  executeQuery_ conn "CREATE INDEX IF NOT EXISTS idx_catalog_albums_current_quality ON catalog_albums(current_quality)"

  -- Add extended metadata fields to library_track_metadata (migration for existing databases)
  addColumnIfNotExists conn "library_track_metadata" "format" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "total_tracks" "INTEGER"
  addColumnIfNotExists conn "library_track_metadata" "total_discs" "INTEGER"
  addColumnIfNotExists conn "library_track_metadata" "publisher" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "comment" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "bits_per_sample" "INTEGER"
  addColumnIfNotExists conn "library_track_metadata" "release_status" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "release_type" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "mb_album_artist_id" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "mb_work_id" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "mb_disc_id" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "acoustid_fingerprint" "TEXT"
  addColumnIfNotExists conn "library_track_metadata" "acoustid_id" "TEXT"

  -- Rename columns for clarity (SQLite doesn't support RENAME COLUMN before 3.25.0, so we check)
  -- Note: These renames are optional and only happen for new databases via CREATE TABLE IF NOT EXISTS
  -- Existing databases will continue to use the old column names (label, country)

  -- Create default quality profiles
  createDefaultQualityProfiles conn

  -- Initialize settings table with default profile
  initializeSettings conn

-- | Helper to add a column to SQLite table if it doesn't already exist.
addColumnIfNotExists :: SQLite.Connection -> Text -> Text -> Text -> IO ()
addColumnIfNotExists conn tableName columnName columnType = do
  -- Check if column exists by querying table info
  rows <- queryRows conn
    "SELECT COUNT(*) FROM pragma_table_info(?) WHERE name = ?"
    (tableName, columnName) :: IO [Only Int]

  case viaNonEmpty head rows of
    Just (Only count) | count == 0 -> do
      -- Column doesn't exist, add it
      let sql = "ALTER TABLE " <> tableName <> " ADD COLUMN " <> columnName <> " " <> columnType
      executeQuery_ conn sql
    _ -> pure ()  -- Column exists, skip

-- | Create the default acquisition source if it doesn't exist.
-- This source tracks all artists found in the user's library.
createDefaultAcquisitionSource :: SQLite.Connection -> IO ()
createDefaultAcquisitionSource conn = do
  -- Check if a library_artists source already exists
  existingSources <- queryRows conn
    "SELECT COUNT(*) FROM acquisition_rules WHERE rule_type = ?"
    (Only (sourceTypeToText LibraryArtists)) :: IO [Only Int]

  case viaNonEmpty head existingSources of
    Just (Only count) | count == 0 -> do
      -- No library_artists source exists, create the default one
      -- Set filters to only track upcoming albums by default
      let defaultFilters = "{\"release_status\":\"upcoming\"}" :: Text
      executeQuery conn
        "INSERT INTO acquisition_rules (name, description, rule_type, enabled, priority, filters) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        ( "Library Artists" :: Text
        , Just ("Track all upcoming albums from artists in your library" :: Text)
        , sourceTypeToText LibraryArtists
        , True
        , 100 :: Int  -- High priority for default source
        , Just defaultFilters
        )
    _ -> pure ()  -- Source already exists, skip

-- | Create "Best New Music" acquisition sources if they don't exist.
-- These are disabled by default but provide curated high-quality album discovery.
createBestNewMusicSources :: SQLite.Connection -> IO ()
createBestNewMusicSources conn = do
  -- Check if Pitchfork Best New Music source already exists
  existingPitchfork <- queryRows conn
    "SELECT COUNT(*) FROM acquisition_rules WHERE name = ?"
    (Only ("Best New Music (Pitchfork)" :: Text)) :: IO [Only Int]

  case viaNonEmpty head existingPitchfork of
    Just (Only count) | count == 0 -> do
      -- Create Pitchfork Best New Music source (disabled by default)
      let pitchforkFilters = "{\"genres\":[\"pop\",\"rock\",\"experimental\",\"electronic\",\"rap\",\"jazz\",\"metal\",\"folkcountry\"],\"min_score\":9.0}" :: Text
      executeQuery conn
        "INSERT INTO acquisition_rules (name, description, rule_type, enabled, priority, filters) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        ( "Best New Music (Pitchfork)" :: Text
        , Just ("Track highly acclaimed albums from Pitchfork (9.0+ score, all genres)" :: Text)
        , sourceTypeToText Pitchfork
        , False  -- Disabled by default
        , 50 :: Int
        , Just pitchforkFilters
        )
    _ -> pure ()  -- Source already exists, skip

  -- Check if Metacritic Best New Music source already exists
  existingMetacritic <- queryRows conn
    "SELECT COUNT(*) FROM acquisition_rules WHERE name = ?"
    (Only ("Best New Music (Metacritic)" :: Text)) :: IO [Only Int]

  case viaNonEmpty head existingMetacritic of
    Just (Only count) | count == 0 -> do
      -- Create Metacritic Best New Music source (disabled by default)
      let metacriticFilters = "{\"genres\":[\"pop\",\"rock\",\"alternative\",\"rap\",\"country\",\"electronic\",\"r&b\",\"jazz\",\"folk\",\"metal\"],\"min_critic_score\":90}" :: Text
      executeQuery conn
        "INSERT INTO acquisition_rules (name, description, rule_type, enabled, priority, filters) \
        \VALUES (?, ?, ?, ?, ?, ?)"
        ( "Best New Music (Metacritic)" :: Text
        , Just ("Track highly acclaimed albums from Metacritic (90+ score, all genres)" :: Text)
        , sourceTypeToText Metacritic
        , False  -- Disabled by default
        , 50 :: Int
        , Just metacriticFilters
        )
    _ -> pure ()  -- Source already exists, skip

-- | Fix existing LibraryArtists sources that have no filters set.
-- This migration ensures that all LibraryArtists sources default to tracking only upcoming albums.
fixDefaultAcquisitionSourceFilters :: SQLite.Connection -> IO ()
fixDefaultAcquisitionSourceFilters conn = do
  -- Find all LibraryArtists sources with NULL or empty filters
  sourcesNeedingFix <- queryRows conn
    "SELECT id, name FROM acquisition_rules WHERE rule_type = ? AND (filters IS NULL OR filters = '')"
    (Only (sourceTypeToText LibraryArtists)) :: IO [(Int64, Text)]

  -- Update each source to have the default upcoming filter
  let defaultFilters = "{\"release_status\":\"upcoming\"}" :: Text
  forM_ sourcesNeedingFix $ \(sourceId, _) -> do
    executeQuery conn
      "UPDATE acquisition_rules SET filters = ? WHERE id = ?"
      (defaultFilters, sourceId)

-- | Create default quality profiles if they don't exist.
createDefaultQualityProfiles :: SQLite.Connection -> IO ()
createDefaultQualityProfiles conn = do
  -- Check if any quality profiles exist
  existingProfiles <- queryRows conn
    "SELECT COUNT(*) FROM quality_profiles"
    () :: IO [Only Int]

  case viaNonEmpty head existingProfiles of
    Just (Only count) | count == 0 -> do
      -- No profiles exist, create the default ones

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

    _ -> pure ()  -- Profiles already exist, skip

-- | Initialize settings table with default values if empty.
initializeSettings :: SQLite.Connection -> IO ()
initializeSettings conn = do
  -- Check if settings row exists
  existingSettings <- queryRows conn
    "SELECT COUNT(*) FROM settings WHERE id = 1"
    () :: IO [Only Int]

  case viaNonEmpty head existingSettings of
    Just (Only count) | count == 0 -> do
      -- No settings row exists, create it
      -- Default to "Lossless Preferred" profile (ID 2)
      executeQuery conn
        "INSERT INTO settings (id, default_quality_profile_id) VALUES (1, 2)"
        ()
    _ -> pure ()  -- Settings already exist, skip
