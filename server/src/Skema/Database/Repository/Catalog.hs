{-# LANGUAGE OverloadedStrings #-}

-- | Catalog operations for artists and albums.
--
-- This module handles catalog_artists and catalog_albums tables.
module Skema.Database.Repository.Catalog
  ( upsertCatalogArtist
  , getCatalogArtists
  , getCatalogArtistByMBID
  , updateCatalogArtistFollowed
  , updateCatalogArtist
  , deleteCatalogArtist
  , upsertCatalogAlbum
  , getCatalogAlbums
  , getCatalogAlbumsByArtistId
  , getCatalogAlbumByReleaseGroupMBID
  , updateCatalogAlbum
  , deleteCatalogAlbum
  , getCatalogAlbumsOverview
  , getCatalogAlbumsOverviewCount
  , getCatalogAlbumsOverviewStats
  , CatalogAlbumOverviewRow(..)
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Only(..))
import Database.SQLite.Simple.ToField (toField)
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T

-- * Catalog artist operations

-- | Upsert a catalog artist (insert or update if exists).
upsertCatalogArtist :: SQLite.Connection -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Maybe Int64 -> Maybe Int64 -> Maybe UTCTime -> IO Int64
upsertCatalogArtist conn artistMBID artistName artistType imageUrl thumbnailUrl followed addedByRuleId sourceClusterId lastCheckedAt =
  insertReturningId conn
    "INSERT INTO catalog_artists (artist_mbid, artist_name, artist_type, image_url, thumbnail_url, followed, added_by_rule_id, source_cluster_id, last_checked_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(artist_mbid) DO UPDATE SET \
    \  artist_name = excluded.artist_name, \
    \  artist_type = excluded.artist_type, \
    \  image_url = COALESCE(excluded.image_url, catalog_artists.image_url), \
    \  thumbnail_url = COALESCE(excluded.thumbnail_url, catalog_artists.thumbnail_url), \
    \  followed = excluded.followed, \
    \  added_by_rule_id = COALESCE(excluded.added_by_rule_id, catalog_artists.added_by_rule_id), \
    \  source_cluster_id = COALESCE(excluded.source_cluster_id, catalog_artists.source_cluster_id), \
    \  last_checked_at = COALESCE(excluded.last_checked_at, catalog_artists.last_checked_at), \
    \  updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (artistMBID, artistName, artistType, imageUrl, thumbnailUrl, followed, addedByRuleId, sourceClusterId, lastCheckedAt)

-- | Get catalog artists, optionally filtered by followed status.
getCatalogArtists :: SQLite.Connection -> Maybe Bool -> IO [CatalogArtistRecord]
getCatalogArtists conn maybeFollowed =
  case maybeFollowed of
    Nothing -> queryRows_ conn
      "SELECT id, artist_mbid, artist_name, artist_type, image_url, thumbnail_url, followed, added_by_rule_id, source_cluster_id, last_checked_at, quality_profile_id, created_at, updated_at \
      \FROM catalog_artists ORDER BY created_at DESC"
    Just followed -> queryRows conn
      "SELECT id, artist_mbid, artist_name, artist_type, image_url, thumbnail_url, followed, added_by_rule_id, source_cluster_id, last_checked_at, quality_profile_id, created_at, updated_at \
      \FROM catalog_artists WHERE followed = ? ORDER BY created_at DESC"
      (Only followed)

-- | Get a catalog artist by MusicBrainz ID.
getCatalogArtistByMBID :: SQLite.Connection -> Text -> IO (Maybe CatalogArtistRecord)
getCatalogArtistByMBID conn artistMBID = do
  results <- queryRows conn
    "SELECT id, artist_mbid, artist_name, artist_type, image_url, thumbnail_url, followed, added_by_rule_id, source_cluster_id, last_checked_at, quality_profile_id, created_at, updated_at \
    \FROM catalog_artists WHERE artist_mbid = ?"
    (Only artistMBID)
  pure $ viaNonEmpty head results

-- | Update the followed status of a catalog artist.
updateCatalogArtistFollowed :: SQLite.Connection -> Int64 -> Bool -> IO ()
updateCatalogArtistFollowed conn artistId followed = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE catalog_artists SET followed = ?, updated_at = ? WHERE id = ?"
    (followed, now, artistId)

-- | Update a catalog artist (followed status and quality profile).
updateCatalogArtist :: SQLite.Connection -> Int64 -> Bool -> Maybe (Maybe Int64) -> IO ()
updateCatalogArtist conn artistId followed maybeProfileId = do
  now <- getCurrentTime
  case maybeProfileId of
    Nothing ->
      -- No change to quality_profile_id
      executeQuery conn
        "UPDATE catalog_artists SET followed = ?, updated_at = ? WHERE id = ?"
        (followed, now, artistId)
    Just Nothing ->
      -- Clear quality_profile_id
      executeQuery conn
        "UPDATE catalog_artists SET followed = ?, quality_profile_id = NULL, updated_at = ? WHERE id = ?"
        (followed, now, artistId)
    Just (Just profileId) ->
      -- Set quality_profile_id
      executeQuery conn
        "UPDATE catalog_artists SET followed = ?, quality_profile_id = ?, updated_at = ? WHERE id = ?"
        (followed, profileId, now, artistId)

-- | Delete a catalog artist.
deleteCatalogArtist :: SQLite.Connection -> Int64 -> IO ()
deleteCatalogArtist conn artistId =
  executeQuery conn "DELETE FROM catalog_artists WHERE id = ?" (Only artistId)

-- * Catalog album operations

-- | Upsert a catalog album (insert or update if exists).
-- NOTE: "wanted" status is no longer stored - it's computed from quality_profile_id + current_quality
upsertCatalogAlbum :: SQLite.Connection -> Text -> Text -> Int64 -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Int64 -> IO Int64
upsertCatalogAlbum conn releaseGroupMBID title artistId artistMBID artistName albumType firstReleaseDate matchedClusterId =
  insertReturningId conn
    "INSERT INTO catalog_albums (release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, matched_cluster_id) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(release_group_mbid) DO UPDATE SET \
    \  title = excluded.title, \
    \  artist_id = excluded.artist_id, \
    \  artist_mbid = excluded.artist_mbid, \
    \  artist_name = excluded.artist_name, \
    \  album_type = excluded.album_type, \
    \  first_release_date = excluded.first_release_date, \
    \  matched_cluster_id = excluded.matched_cluster_id, \
    \  updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (releaseGroupMBID, title, artistId, artistMBID, artistName, albumType, firstReleaseDate, matchedClusterId)

-- | Get catalog albums (no filtering - wanted status is computed, not stored).
-- Use getCatalogAlbumsByArtistId to filter by artist.
getCatalogAlbums :: SQLite.Connection -> Maybe Bool -> IO [CatalogAlbumRecord]
getCatalogAlbums conn _ =
  -- NOTE: maybeWanted parameter is kept for backwards compatibility but ignored
  -- "wanted" status is now computed from quality_profile_id + current_quality, not stored
  queryRows_ conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums ORDER BY created_at DESC"

-- | Get catalog albums by internal artist ID (no wanted filtering - it's computed).
getCatalogAlbumsByArtistId :: SQLite.Connection -> Int64 -> Maybe Bool -> IO [CatalogAlbumRecord]
getCatalogAlbumsByArtistId conn artistId _ =
  -- NOTE: maybeWanted parameter is kept for backwards compatibility but ignored
  queryRows conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums WHERE artist_id = ? ORDER BY first_release_date DESC"
    (Only artistId)

-- | Get a catalog album by release group MusicBrainz ID.
getCatalogAlbumByReleaseGroupMBID :: SQLite.Connection -> Text -> IO (Maybe CatalogAlbumRecord)
getCatalogAlbumByReleaseGroupMBID conn releaseGroupMBID = do
  results <- queryRows conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums WHERE release_group_mbid = ?"
    (Only releaseGroupMBID)
  pure $ viaNonEmpty head results

-- | Update a catalog album's quality profile.
-- NOTE: "wanted" status is NOT stored anymore - it's derived from the quality profile.
-- This function only updates the quality_profile_id.
updateCatalogAlbum :: SQLite.Connection -> Int64 -> Maybe (Maybe Int64) -> IO ()
updateCatalogAlbum conn albumId maybeProfileId = do
  now <- getCurrentTime
  case maybeProfileId of
    Nothing ->
      -- No change to quality_profile_id, just update timestamp
      executeQuery conn
        "UPDATE catalog_albums SET updated_at = ? WHERE id = ?"
        (now, albumId)
    Just Nothing ->
      -- Clear quality_profile_id (album becomes "not wanted")
      executeQuery conn
        "UPDATE catalog_albums SET quality_profile_id = NULL, updated_at = ? WHERE id = ?"
        (now, albumId)
    Just (Just profileId) ->
      -- Set quality_profile_id (album becomes "wanted" if quality requirements not met)
      executeQuery conn
        "UPDATE catalog_albums SET quality_profile_id = ?, updated_at = ? WHERE id = ?"
        (profileId, now, albumId)

-- | Delete a catalog album.
deleteCatalogAlbum :: SQLite.Connection -> Int64 -> IO ()
deleteCatalogAlbum conn albumId =
  executeQuery conn "DELETE FROM catalog_albums WHERE id = ?" (Only albumId)

-- * Catalog albums overview queries (with joins and state computation)

-- | Row type for catalog album overview query result.
-- This includes all fields needed to construct CatalogAlbumOverviewResponse.
data CatalogAlbumOverviewRow = CatalogAlbumOverviewRow
  { caorAlbumId :: Int64
  , caorReleaseGroupMBID :: Text
  , caorTitle :: Text
  , caorArtistId :: Maybe Int64
  , caorArtistMBID :: Text
  , caorArtistName :: Text
  , caorAlbumType :: Maybe Text
  , caorFirstReleaseDate :: Maybe Text
  , caorCoverUrl :: Maybe Text
  , caorCoverThumbnailUrl :: Maybe Text
  , caorWanted :: Bool
  , caorMatchedClusterId :: Maybe Int64
  , caorCurrentQuality :: Maybe Text
  , caorQualityProfileId :: Maybe Int64
  , caorQualityProfileName :: Maybe Text
  , caorQualityProfileCutoff :: Maybe Text
  , caorActiveDownloadId :: Maybe Int64
  , caorActiveDownloadStatus :: Maybe Text
  , caorActiveDownloadProgress :: Maybe Double
  , caorActiveDownloadQuality :: Maybe Text
  , caorActiveDownloadTitle :: Maybe Text
  , caorActiveDownloadSizeBytes :: Maybe Int64
  , caorActiveDownloadStartedAt :: Maybe Text
  , caorActiveDownloadErrorMessage :: Maybe Text
  , caorDownloadCount :: Int
  , caorLastDownloadAt :: Maybe Text
  , caorCreatedAt :: Text
  , caorUpdatedAt :: Text
  , caorImportedAt :: Maybe Text
  } deriving (Show)

instance SQLite.FromRow CatalogAlbumOverviewRow where
  fromRow = CatalogAlbumOverviewRow
    <$> SQLite.field  -- album_id
    <*> SQLite.field  -- release_group_mbid
    <*> SQLite.field  -- title
    <*> SQLite.field  -- artist_id
    <*> SQLite.field  -- artist_mbid
    <*> SQLite.field  -- artist_name
    <*> SQLite.field  -- album_type
    <*> SQLite.field  -- first_release_date
    <*> SQLite.field  -- cover_url
    <*> SQLite.field  -- cover_thumbnail_url
    <*> SQLite.field  -- wanted
    <*> SQLite.field  -- matched_cluster_id
    <*> SQLite.field  -- current_quality
    <*> SQLite.field  -- quality_profile_id
    <*> SQLite.field  -- quality_profile_name
    <*> SQLite.field  -- quality_profile_cutoff
    <*> SQLite.field  -- active_download_id
    <*> SQLite.field  -- active_download_status
    <*> SQLite.field  -- active_download_progress
    <*> SQLite.field  -- active_download_quality
    <*> SQLite.field  -- active_download_title
    <*> SQLite.field  -- active_download_size_bytes
    <*> SQLite.field  -- active_download_started_at
    <*> SQLite.field  -- active_download_error_message
    <*> SQLite.field  -- download_count
    <*> SQLite.field  -- last_download_at
    <*> SQLite.field  -- created_at
    <*> SQLite.field  -- updated_at
    <*> SQLite.field  -- imported_at

-- | Get catalog albums overview with enhanced information (joins, downloads, state).
--
-- This query joins catalog_albums with:
-- - quality_profiles (for profile names)
-- - downloads (for active download info and counts)
-- - clusters (for import timestamps)
getCatalogAlbumsOverview
  :: SQLite.Connection
  -> Int        -- ^ Limit
  -> Int        -- ^ Offset
  -> Maybe [Text]  -- ^ Filter by state (not yet implemented)
  -> Maybe [Text]  -- ^ Filter by quality
  -> Maybe Int64   -- ^ Filter by artist ID
  -> Maybe Text    -- ^ Search query
  -> Maybe Text    -- ^ Sort field
  -> Maybe Text    -- ^ Sort order (asc/desc)
  -> IO [CatalogAlbumOverviewRow]
getCatalogAlbumsOverview conn limit offset _maybeStates maybeQualities maybeArtistId maybeSearch maybeSort maybeOrder = do
  let baseQuery =
        "SELECT \
        \  ca.id, \
        \  ca.release_group_mbid, \
        \  ca.title, \
        \  ca.artist_id, \
        \  ca.artist_mbid, \
        \  ca.artist_name, \
        \  ca.album_type, \
        \  ca.first_release_date, \
        \  ca.album_cover_url, \
        \  ca.album_cover_thumbnail_url, \
        \  ca.wanted, \
        \  ca.matched_cluster_id, \
        \  ca.current_quality, \
        \  ca.quality_profile_id, \
        \  qp.name, \
        \  qp.cutoff_quality, \
        \  d_active.id, \
        \  d_active.status, \
        \  d_active.progress, \
        \  d_active.quality, \
        \  d_active.title, \
        \  d_active.size_bytes, \
        \  d_active.started_at, \
        \  d_active.error_message, \
        \  COALESCE(d_count.download_count, 0), \
        \  d_count.last_download_at, \
        \  CURRENT_TIMESTAMP, \
        \  CURRENT_TIMESTAMP, \
        \  NULL \
        \FROM catalog_albums ca \
        \LEFT JOIN quality_profiles qp ON ca.quality_profile_id = qp.id \
        \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
        \  AND d_active.status IN ('queued', 'downloading', 'processing') \
        \LEFT JOIN ( \
        \  SELECT catalog_album_id, COUNT(*) as download_count, MAX(queued_at) as last_download_at \
        \  FROM downloads \
        \  GROUP BY catalog_album_id \
        \) d_count ON ca.id = d_count.catalog_album_id \
        \LEFT JOIN clusters c ON ca.matched_cluster_id = c.id \
        \WHERE 1=1 "

  -- Build filter conditions
  let (whereClause, params) = buildOverviewFilters maybeQualities maybeArtistId maybeSearch
  let sortClause = buildSortClause maybeSort maybeOrder
  let fullQuery = baseQuery <> whereClause <> sortClause <> " LIMIT ? OFFSET ?"

  queryRows conn fullQuery (params <> [toField limit, toField offset])

-- | Get total count of albums matching filters.
getCatalogAlbumsOverviewCount
  :: SQLite.Connection
  -> Maybe [Text]  -- ^ Filter by quality
  -> Maybe Int64   -- ^ Filter by artist ID
  -> Maybe Text    -- ^ Search query
  -> IO Int
getCatalogAlbumsOverviewCount conn maybeQualities maybeArtistId maybeSearch = do
  let baseQuery = "SELECT COUNT(*) FROM catalog_albums ca WHERE 1=1 "
  let (whereClause, params) = buildOverviewFilters maybeQualities maybeArtistId maybeSearch
  let fullQuery = baseQuery <> whereClause

  results <- queryRows conn fullQuery params :: IO [Only Int]
  pure $ maybe 0 (\(Only n) -> n) (viaNonEmpty head results)

-- | Get statistics by state and quality.
getCatalogAlbumsOverviewStats
  :: SQLite.Connection
  -> IO [(Text, Int)]  -- ^ Stats by quality (state stats TODO)
getCatalogAlbumsOverviewStats conn = do
  -- For now, just return quality stats
  -- TODO: Implement state computation and group by state
  queryRows_ conn
    "SELECT COALESCE(current_quality, 'unknown'), COUNT(*) \
    \FROM catalog_albums \
    \GROUP BY current_quality"

-- Helper function to build filter WHERE clauses
buildOverviewFilters :: Maybe [Text] -> Maybe Int64 -> Maybe Text -> (Text, [SQLite.SQLData])
buildOverviewFilters maybeQualities maybeArtistId maybeSearch =
  let (clauses, params) = mconcat
        [ case maybeQualities of
            Nothing -> ([], [])
            Just qualities ->
              let placeholders = T.intercalate "," (replicate (length qualities) "?")
              in (["AND ca.current_quality IN (" <> placeholders <> ")"], map toField qualities)
        , case maybeArtistId of
            Nothing -> ([], [])
            Just artistId -> (["AND ca.artist_id = ?"], [toField artistId])
        , case maybeSearch of
            Nothing -> ([], [])
            Just query ->
              (["AND (ca.title LIKE ? OR ca.artist_name LIKE ?)"],
               [toField ("%" <> query <> "%"), toField ("%" <> query <> "%")])
        ]
  in (T.unwords clauses, params)

-- Helper function to build ORDER BY clause
buildSortClause :: Maybe Text -> Maybe Text -> Text
buildSortClause maybeSort maybeOrder =
  let order = case maybeOrder of
        Just "asc" -> " ASC"
        Just "desc" -> " DESC"
        _ -> " DESC"  -- Default to descending
      sortField = case maybeSort of
        Just "title" -> "ca.title"
        Just "artist" -> "ca.artist_name"
        Just "date" -> "ca.first_release_date"
        Just "quality" -> "ca.current_quality"
        Just "state" -> "ca.id"  -- State is computed, sort by ID
        _ -> "ca.id"  -- Default: by ID
  in " ORDER BY " <> sortField <> order
