{-# LANGUAGE OverloadedStrings #-}

-- | Overview queries for catalog albums.
--
-- This module handles complex overview queries with joins and state computation.
module Skema.Database.Repository.Catalog.Overview
  ( -- * Overview types
    CatalogAlbumOverviewRow(..)
    -- * Overview queries
  , getCatalogAlbumsOverview
  , getCatalogAlbumsOverviewCount
  , getCatalogAlbumsOverviewStats
  , getCatalogAlbumsByArtistOverview
  , getCatalogAlbumsStatsByState
  ) where

import Skema.Database.Repository.Catalog.Types
import Skema.Database.Repository.Catalog.Artist (normalizeForSearch)

import Skema.Database.Connection
import Database.SQLite.Simple (Only(..))
import Database.SQLite.Simple.ToField (toField)
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T

-- * Overview types

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

-- * Overview queries

-- | Get catalog albums overview with enhanced information (joins, downloads, state).
--
-- This query joins catalog_albums with:
-- - quality_profiles (for profile names)
-- - downloads (for active download info and counts)
-- - clusters (for import timestamps)
getCatalogAlbumsOverview
  :: SQLite.Connection
  -> AlbumQuery
  -> IO [CatalogAlbumOverviewRow]
getCatalogAlbumsOverview conn query = do
  let limit = aqLimit query
      offset = aqOffset query
      maybeStates = aqStates query
      maybeQualities = aqQualities query
      maybeArtistId = aqArtistId query
      maybeSearch = aqSearch query
      maybeSort = aqSort query
      maybeOrder = aqOrder query
      maybeReleaseDateAfter = aqReleaseDateAfter query
      maybeReleaseDateBefore = aqReleaseDateBefore query
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
        \  CASE \
        \    WHEN ca.quality_profile_id IS NULL THEN 0 \
        \    WHEN c.id IS NULL THEN 1 \
        \    WHEN ca.current_quality IS NULL THEN 1 \
        \    WHEN qp.cutoff_quality IS NULL THEN 0 \
        \    ELSE CASE \
        \      WHEN ca.current_quality = 'FLAC' AND qp.cutoff_quality IN ('MP3', 'V0', 'FLAC') THEN 0 \
        \      WHEN ca.current_quality = 'V0' AND qp.cutoff_quality IN ('MP3', 'V0') THEN 0 \
        \      WHEN ca.current_quality = 'MP3' AND qp.cutoff_quality = 'MP3' THEN 0 \
        \      ELSE 1 \
        \    END \
        \  END AS wanted, \
        \  c.id, \
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
        \  AND d_active.status IN ('queued', 'downloading', 'processing', 'failed', 'identification_failure') \
        \LEFT JOIN ( \
        \  SELECT catalog_album_id, COUNT(*) as download_count, MAX(queued_at) as last_download_at \
        \  FROM downloads \
        \  GROUP BY catalog_album_id \
        \) d_count ON ca.id = d_count.catalog_album_id \
        \LEFT JOIN ( \
        \  SELECT mb_release_group_id, MIN(id) as id \
        \  FROM clusters \
        \  WHERE mb_release_group_id IS NOT NULL \
        \  GROUP BY mb_release_group_id \
        \) c ON c.mb_release_group_id = ca.release_group_mbid \
        \WHERE 1=1 "

  -- Build filter conditions
  let (whereClause, params) = buildOverviewFilters maybeQualities maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore

  -- Wrap the base query in a subquery if we need to filter by state
  let (finalQuery, finalParams, sortClause) = case maybeStates of
        Nothing ->
          (baseQuery <> whereClause, params, buildSortClause maybeSort maybeOrder)
        Just states ->
          -- Wrap in subquery with computed state column
          -- Only select the 29 columns needed for CatalogAlbumOverviewRow (exclude computed_state from final SELECT)
          let subquery = "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, \
                         \album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, \
                         \wanted, cluster_id, current_quality, quality_profile_id, profile_name, cutoff_quality, \
                         \active_download_id, active_download_status, progress, download_quality, download_title, \
                         \size_bytes, started_at, error_message, download_count, last_download_at, \
                         \created_at, updated_at, imported_at \
                         \FROM (\
                         \SELECT \
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
                         \  CASE \
                         \    WHEN ca.quality_profile_id IS NULL THEN 0 \
                         \    WHEN c.id IS NULL THEN 1 \
                         \    WHEN ca.current_quality IS NULL THEN 1 \
                         \    WHEN qp.cutoff_quality IS NULL THEN 0 \
                         \    ELSE CASE \
                         \      WHEN ca.current_quality = 'FLAC' AND qp.cutoff_quality IN ('MP3', 'V0', 'FLAC') THEN 0 \
                         \      WHEN ca.current_quality = 'V0' AND qp.cutoff_quality IN ('MP3', 'V0') THEN 0 \
                         \      WHEN ca.current_quality = 'MP3' AND qp.cutoff_quality = 'MP3' THEN 0 \
                         \      ELSE 1 \
                         \    END \
                         \  END AS wanted, \
                         \  c.id AS cluster_id, \
                         \  ca.current_quality, \
                         \  ca.quality_profile_id, \
                         \  qp.name AS profile_name, \
                         \  qp.cutoff_quality, \
                         \  d_active.id AS active_download_id, \
                         \  d_active.status AS active_download_status, \
                         \  d_active.progress, \
                         \  d_active.quality AS download_quality, \
                         \  d_active.title AS download_title, \
                         \  d_active.size_bytes, \
                         \  d_active.started_at, \
                         \  d_active.error_message, \
                         \  COALESCE(d_count.download_count, 0) AS download_count, \
                         \  d_count.last_download_at, \
                         \  CURRENT_TIMESTAMP AS created_at, \
                         \  CURRENT_TIMESTAMP AS updated_at, \
                         \  NULL AS imported_at, \
                         \  CASE \
                         \    WHEN ca.quality_profile_id IS NULL AND c.id IS NULL THEN 'NotWanted' \
                         \    WHEN ca.quality_profile_id IS NULL AND c.id IS NOT NULL THEN 'InLibrary' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NULL THEN 'Wanted' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'queued' THEN 'Searching' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status IN ('downloading', 'processing') THEN 'Downloading' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'failed' THEN 'Failed' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'identification_failure' THEN 'IdentificationFailed' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NULL THEN 'Monitored' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NOT NULL THEN 'Upgrading' \
                         \    ELSE 'Wanted' \
                         \  END AS computed_state \
                         \FROM catalog_albums ca \
                         \LEFT JOIN quality_profiles qp ON ca.quality_profile_id = qp.id \
                         \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
                         \  AND d_active.status IN ('queued', 'downloading', 'processing', 'failed', 'identification_failure') \
                         \LEFT JOIN ( \
                         \  SELECT catalog_album_id, COUNT(*) as download_count, MAX(queued_at) as last_download_at \
                         \  FROM downloads \
                         \  GROUP BY catalog_album_id \
                         \) d_count ON ca.id = d_count.catalog_album_id \
                         \LEFT JOIN ( \
                         \  SELECT mb_release_group_id, MIN(id) as id \
                         \  FROM clusters \
                         \  WHERE mb_release_group_id IS NOT NULL \
                         \  GROUP BY mb_release_group_id \
                         \) c ON c.mb_release_group_id = ca.release_group_mbid \
                         \WHERE 1=1 " <> whereClause <> ") sub WHERE computed_state IN (" <> T.intercalate "," (replicate (length states) "?") <> ")"
              stateParams = map toField states
              sortClauseForSub = buildSortClauseForSubquery maybeSort maybeOrder
          in (subquery, params <> stateParams, sortClauseForSub)

  let fullQuery = finalQuery <> sortClause <> " LIMIT ? OFFSET ?"

  queryRows conn fullQuery (finalParams <> [toField limit, toField offset])

-- | Get total count of albums matching filters.
getCatalogAlbumsOverviewCount
  :: SQLite.Connection
  -> AlbumQuery
  -> IO Int
getCatalogAlbumsOverviewCount conn query = do
  let maybeStates = aqStates query
      maybeQualities = aqQualities query
      maybeArtistId = aqArtistId query
      maybeSearch = aqSearch query
      maybeReleaseDateAfter = aqReleaseDateAfter query
      maybeReleaseDateBefore = aqReleaseDateBefore query
  let (whereClause, params) = buildOverviewFilters maybeQualities maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore

  let (fullQuery, finalParams) = case maybeStates of
        Nothing ->
          ("SELECT COUNT(*) FROM catalog_albums ca WHERE 1=1 " <> whereClause, params)
        Just states ->
          -- Use subquery with computed state for filtering
          let subquery = "SELECT COUNT(*) FROM (\
                         \SELECT ca.id, \
                         \  CASE \
                         \    WHEN ca.quality_profile_id IS NULL AND c.id IS NULL THEN 'NotWanted' \
                         \    WHEN ca.quality_profile_id IS NULL AND c.id IS NOT NULL THEN 'InLibrary' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NULL THEN 'Wanted' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'queued' THEN 'Searching' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status IN ('downloading', 'processing') THEN 'Downloading' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'failed' THEN 'Failed' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'identification_failure' THEN 'IdentificationFailed' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NULL THEN 'Monitored' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NOT NULL THEN 'Upgrading' \
                         \    ELSE 'Wanted' \
                         \  END AS computed_state \
                         \FROM catalog_albums ca \
                         \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
                         \  AND d_active.status IN ('queued', 'downloading', 'processing', 'failed', 'identification_failure') \
                         \LEFT JOIN clusters c ON c.mb_release_group_id = ca.release_group_mbid \
                         \WHERE 1=1 " <> whereClause <> ") sub WHERE computed_state IN (" <> T.intercalate "," (replicate (length states) "?") <> ")"
              stateParams = map toField states
          in (subquery, params <> stateParams)

  results <- queryRows conn fullQuery finalParams :: IO [Only Int]
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

-- | Get all catalog albums for an artist with full overview data.
-- This function returns all albums for a specific artist without pagination,
-- sorted by first release date descending.
getCatalogAlbumsByArtistOverview
  :: SQLite.Connection
  -> Int64  -- ^ Artist ID
  -> IO [CatalogAlbumOverviewRow]
getCatalogAlbumsByArtistOverview conn artistId = do
  let query =
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
        \  CASE \
        \    WHEN ca.quality_profile_id IS NULL THEN 0 \
        \    WHEN c.id IS NULL THEN 1 \
        \    WHEN ca.current_quality IS NULL THEN 1 \
        \    WHEN qp.cutoff_quality IS NULL THEN 0 \
        \    ELSE CASE \
        \      WHEN ca.current_quality = 'FLAC' AND qp.cutoff_quality IN ('MP3', 'V0', 'FLAC') THEN 0 \
        \      WHEN ca.current_quality = 'V0' AND qp.cutoff_quality IN ('MP3', 'V0') THEN 0 \
        \      WHEN ca.current_quality = 'MP3' AND qp.cutoff_quality = 'MP3' THEN 0 \
        \      ELSE 1 \
        \    END \
        \  END AS wanted, \
        \  c.id, \
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
        \  AND d_active.status IN ('queued', 'downloading', 'processing', 'failed', 'identification_failure') \
        \LEFT JOIN ( \
        \  SELECT catalog_album_id, COUNT(*) as download_count, MAX(queued_at) as last_download_at \
        \  FROM downloads \
        \  GROUP BY catalog_album_id \
        \) d_count ON ca.id = d_count.catalog_album_id \
        \LEFT JOIN ( \
        \  SELECT mb_release_group_id, MIN(id) as id \
        \  FROM clusters \
        \  WHERE mb_release_group_id IS NOT NULL \
        \  GROUP BY mb_release_group_id \
        \) c ON c.mb_release_group_id = ca.release_group_mbid \
        \WHERE ca.artist_id = ? \
        \ORDER BY ca.first_release_date DESC"
  queryRows conn query (Only artistId)

-- | Get album count statistics grouped by computed state.
-- This uses SQL-side aggregation to efficiently compute state counts
-- without fetching all albums into memory.
getCatalogAlbumsStatsByState
  :: SQLite.Connection
  -> Maybe Int64    -- ^ Optional artist ID filter
  -> Maybe Text     -- ^ Optional search query
  -> Maybe Text     -- ^ Optional release date after filter
  -> Maybe Text     -- ^ Optional release date before filter
  -> IO [(Text, Int)]  -- ^ List of (state name, count)
getCatalogAlbumsStatsByState conn maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore = do
  let (whereClause, params) = buildStatsFilters maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore
  let query =
        "SELECT computed_state, COUNT(*) as cnt FROM ( \
        \SELECT \
        \  CASE \
        \    WHEN ca.quality_profile_id IS NULL AND c.id IS NULL THEN 'NotWanted' \
        \    WHEN ca.quality_profile_id IS NULL AND c.id IS NOT NULL THEN 'InLibrary' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NULL THEN 'Wanted' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'queued' THEN 'Searching' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status IN ('downloading', 'processing') THEN 'Downloading' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'failed' THEN 'Failed' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'identification_failure' THEN 'IdentificationFailed' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NULL THEN 'Monitored' \
        \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NOT NULL THEN 'Upgrading' \
        \    ELSE 'Wanted' \
        \  END AS computed_state \
        \FROM catalog_albums ca \
        \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
        \  AND d_active.status IN ('queued', 'downloading', 'processing', 'failed', 'identification_failure') \
        \LEFT JOIN ( \
        \  SELECT mb_release_group_id, MIN(id) as id \
        \  FROM clusters \
        \  WHERE mb_release_group_id IS NOT NULL \
        \  GROUP BY mb_release_group_id \
        \) c ON c.mb_release_group_id = ca.release_group_mbid \
        \WHERE 1=1 " <> whereClause <> "\
        \) sub \
        \GROUP BY computed_state"
  queryRows conn query params

-- * Helper functions

-- Helper function to build filter WHERE clauses
buildOverviewFilters :: Maybe [Text] -> Maybe Int64 -> Maybe Text -> Maybe Text -> Maybe Text -> (Text, [SQLite.SQLData])
buildOverviewFilters maybeQualities maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore =
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
              -- Split query into words and search for each word in either title or artist
              let words = T.words query
                  normalizedWords = filter (not . T.null) $ map normalizeForSearch words
                  -- Each word must match in either title or artist
                  wordClauses = map (\_ -> "(ca.title_normalized LIKE ? OR ca.artist_name_normalized LIKE ?)") normalizedWords
                  wordParams = concatMap (\w -> [toField ("%" <> w <> "%"), toField ("%" <> w <> "%")]) normalizedWords
              in if null normalizedWords
                   then ([], [])
                   else (["AND (" <> T.intercalate " AND " wordClauses <> ")"], wordParams)
        , case maybeReleaseDateAfter of
            Nothing -> ([], [])
            Just dateValue ->
              if dateValue == "today"
                then (["AND date(ca.first_release_date) >= date('now')"], [])
                else (["AND date(ca.first_release_date) >= ?"], [toField dateValue])
        , case maybeReleaseDateBefore of
            Nothing -> ([], [])
            Just dateValue ->
              if dateValue == "today"
                then (["AND date(ca.first_release_date) <= date('now')"], [])
                else (["AND date(ca.first_release_date) <= ?"], [toField dateValue])
        ]
  in (T.unwords clauses, params)

-- Helper function to build ORDER BY clause
-- When inSubquery is True, use column aliases without table prefix
buildSortClause :: Maybe Text -> Maybe Text -> Text
buildSortClause maybeSort maybeOrder =
  buildSortClauseInternal maybeSort maybeOrder False

buildSortClauseForSubquery :: Maybe Text -> Maybe Text -> Text
buildSortClauseForSubquery maybeSort maybeOrder =
  buildSortClauseInternal maybeSort maybeOrder True

buildSortClauseInternal :: Maybe Text -> Maybe Text -> Bool -> Text
buildSortClauseInternal maybeSort maybeOrder inSubquery =
  let order = case maybeOrder of
        Just "asc" -> " ASC"
        Just "desc" -> " DESC"
        _ -> " DESC"  -- Default to descending
      sortField = case maybeSort of
        Just "title" -> if inSubquery then "title" else "ca.title"
        Just "artist" -> if inSubquery then "artist_name" else "ca.artist_name"
        Just "date" -> if inSubquery then "first_release_date" else "ca.first_release_date"
        Just "first_release_date" -> if inSubquery then "first_release_date" else "ca.first_release_date"
        Just "quality" -> if inSubquery then "current_quality" else "ca.current_quality"
        Just "state" -> if inSubquery then "id" else "ca.id"
        _ -> if inSubquery then "id" else "ca.id"  -- Default: by ID
  in " ORDER BY " <> sortField <> order

-- Helper function to build filter WHERE clauses for stats query
buildStatsFilters :: Maybe Int64 -> Maybe Text -> Maybe Text -> Maybe Text -> (Text, [SQLite.SQLData])
buildStatsFilters maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore =
  let (clauses, params) = mconcat
        [ case maybeArtistId of
            Nothing -> ([], [])
            Just artistId -> (["AND ca.artist_id = ?"], [toField artistId])
        , case maybeSearch of
            Nothing -> ([], [])
            Just query ->
              let words = T.words query
                  normalizedWords = filter (not . T.null) $ map normalizeForSearch words
                  wordClauses = map (\_ -> "(ca.title_normalized LIKE ? OR ca.artist_name_normalized LIKE ?)") normalizedWords
                  wordParams = concatMap (\w -> [toField ("%" <> w <> "%"), toField ("%" <> w <> "%")]) normalizedWords
              in if null normalizedWords
                   then ([], [])
                   else (["AND (" <> T.intercalate " AND " wordClauses <> ")"], wordParams)
        , case maybeReleaseDateAfter of
            Nothing -> ([], [])
            Just dateValue ->
              if dateValue == "today"
                then (["AND date(ca.first_release_date) >= date('now')"], [])
                else (["AND date(ca.first_release_date) >= ?"], [toField dateValue])
        , case maybeReleaseDateBefore of
            Nothing -> ([], [])
            Just dateValue ->
              if dateValue == "today"
                then (["AND date(ca.first_release_date) <= date('now')"], [])
                else (["AND date(ca.first_release_date) <= ?"], [toField dateValue])
        ]
  in (T.unwords clauses, params)
