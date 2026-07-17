{-# LANGUAGE OverloadedStrings #-}

-- | Overview queries for catalog albums.
--
-- All overview queries are composed from one base SELECT
-- ('overviewBaseSelect') so the joins, the wanted computation, and the
-- computed state cannot drift between the listing, count, per-artist, and
-- stats variants. The computed state is calculated only here (in SQL);
-- handlers read it from the row instead of re-deriving it.
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
import Skema.Database.Utils (albumWantedSql)
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
  , caorState :: Text
    -- ^ The computed album state (single source: the SQL CASE below)
  } deriving (Show)

instance SQLite.FromRow CatalogAlbumOverviewRow where
  fromRow = CatalogAlbumOverviewRow
    <$> SQLite.field  -- id
    <*> SQLite.field  -- release_group_mbid
    <*> SQLite.field  -- title
    <*> SQLite.field  -- artist_id
    <*> SQLite.field  -- artist_mbid
    <*> SQLite.field  -- artist_name
    <*> SQLite.field  -- album_type
    <*> SQLite.field  -- first_release_date
    <*> SQLite.field  -- album_cover_url
    <*> SQLite.field  -- album_cover_thumbnail_url
    <*> SQLite.field  -- wanted
    <*> SQLite.field  -- cluster_id
    <*> SQLite.field  -- current_quality
    <*> SQLite.field  -- quality_profile_id
    <*> SQLite.field  -- profile_name
    <*> SQLite.field  -- cutoff_quality
    <*> SQLite.field  -- active_download_id
    <*> SQLite.field  -- active_download_status
    <*> SQLite.field  -- progress
    <*> SQLite.field  -- download_quality
    <*> SQLite.field  -- download_title
    <*> SQLite.field  -- size_bytes
    <*> SQLite.field  -- started_at
    <*> SQLite.field  -- error_message
    <*> SQLite.field  -- download_count
    <*> SQLite.field  -- last_download_at
    <*> SQLite.field  -- created_at
    <*> SQLite.field  -- updated_at
    <*> SQLite.field  -- imported_at
    <*> SQLite.field  -- computed_state

-- * The single base query

-- | Download statuses that make a download the album's "active" one.
activeStatusesSql :: Text
activeStatusesSql = "('queued', 'downloading', 'processing', 'failed', 'identification_failure')"

-- | The computed album state, mirroring the AlbumState constructors.
computedStateSql :: Text
computedStateSql =
  "CASE \
  \  WHEN ca.quality_profile_id IS NULL AND c.quality IS NULL THEN 'NotWanted' \
  \  WHEN ca.quality_profile_id IS NULL AND c.quality IS NOT NULL THEN 'InLibrary' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NULL AND d_active.id IS NULL THEN 'Wanted' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'queued' THEN 'Searching' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NULL AND d_active.id IS NOT NULL AND d_active.status IN ('downloading', 'processing') THEN 'Downloading' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'failed' THEN 'Failed' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'identification_failure' THEN 'IdentificationFailed' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NOT NULL AND d_active.id IS NULL THEN 'Monitored' \
  \  WHEN ca.quality_profile_id IS NOT NULL AND c.quality IS NOT NULL AND d_active.id IS NOT NULL THEN 'Upgrading' \
  \  ELSE 'Wanted' \
  \END"

-- | The full overview SELECT: catalog_albums joined with the album's cluster
-- (via the clusters.catalog_album_id FK), quality profile, active download,
-- and download counts. Ends with @WHERE 1=1@ so filter clauses can be
-- appended; wrap it in a subquery to filter or sort by computed columns.
overviewBaseSelect :: Text
overviewBaseSelect =
  "SELECT \
  \  ca.id AS id, \
  \  ca.release_group_mbid AS release_group_mbid, \
  \  ca.title AS title, \
  \  ca.artist_id AS artist_id, \
  \  ca.artist_mbid AS artist_mbid, \
  \  ca.artist_name AS artist_name, \
  \  ca.album_type AS album_type, \
  \  ca.first_release_date AS first_release_date, \
  \  ca.album_cover_url AS album_cover_url, \
  \  ca.album_cover_thumbnail_url AS album_cover_thumbnail_url, \
  \  " <> albumWantedSql <> " AS wanted, \
  \  c.id AS cluster_id, \
  \  c.quality AS current_quality, \
  \  ca.quality_profile_id AS quality_profile_id, \
  \  qp.name AS profile_name, \
  \  qp.cutoff_quality AS cutoff_quality, \
  \  d_active.id AS active_download_id, \
  \  d_active.status AS active_download_status, \
  \  d_active.progress AS progress, \
  \  d_active.quality AS download_quality, \
  \  d_active.title AS download_title, \
  \  d_active.size_bytes AS size_bytes, \
  \  d_active.started_at AS started_at, \
  \  d_active.error_message AS error_message, \
  \  COALESCE(d_count.download_count, 0) AS download_count, \
  \  d_count.last_download_at AS last_download_at, \
  \  ca.created_at AS created_at, \
  \  ca.updated_at AS updated_at, \
  \  c.created_at AS imported_at, \
  \  " <> computedStateSql <> " AS computed_state \
  \FROM catalog_albums ca \
  \LEFT JOIN quality_profiles qp ON ca.quality_profile_id = qp.id \
  \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
  \  AND d_active.status IN " <> activeStatusesSql <> " \
  \  AND d_active.id = (SELECT MAX(d2.id) FROM downloads d2 WHERE d2.catalog_album_id = ca.id AND d2.status IN " <> activeStatusesSql <> ") \
  \LEFT JOIN ( \
  \  SELECT catalog_album_id, COUNT(*) as download_count, MAX(queued_at) as last_download_at \
  \  FROM downloads \
  \  GROUP BY catalog_album_id \
  \) d_count ON ca.id = d_count.catalog_album_id \
  \LEFT JOIN clusters c ON c.id = ( \
  \  SELECT MAX(c2.id) FROM clusters c2 WHERE c2.catalog_album_id = ca.id \
  \) \
  \WHERE 1=1 "

-- | The optional state filter, applied outside the subquery.
buildStateClause :: Maybe [Text] -> (Text, [SQLite.SQLData])
buildStateClause Nothing = ("", [])
buildStateClause (Just states) =
  ( " WHERE computed_state IN (" <> T.intercalate "," (replicate (length states) "?") <> ")"
  , map toField states
  )

-- * Overview queries

-- | Get catalog albums overview with enhanced information (joins, downloads, state).
getCatalogAlbumsOverview
  :: SQLite.Connection
  -> AlbumQuery
  -> IO [CatalogAlbumOverviewRow]
getCatalogAlbumsOverview conn query = do
  let (whereClause, params) = buildOverviewFilters (aqAlbumId query) (aqQualities query) (aqArtistId query) (aqSearch query) (aqReleaseDateAfter query) (aqReleaseDateBefore query) (aqReleaseGroupMBID query)
      (stateClause, stateParams) = buildStateClause (aqStates query)
      fullQuery =
        "SELECT * FROM (" <> overviewBaseSelect <> whereClause <> ") sub"
          <> stateClause
          <> buildSortClause (aqSort query) (aqOrder query)
          <> " LIMIT ? OFFSET ?"
  queryRows conn fullQuery (params <> stateParams <> [toField (aqLimit query), toField (aqOffset query)])

-- | Get total count of albums matching filters.
getCatalogAlbumsOverviewCount
  :: SQLite.Connection
  -> AlbumQuery
  -> IO Int
getCatalogAlbumsOverviewCount conn query = do
  let (whereClause, params) = buildOverviewFilters (aqAlbumId query) (aqQualities query) (aqArtistId query) (aqSearch query) (aqReleaseDateAfter query) (aqReleaseDateBefore query) (aqReleaseGroupMBID query)
      (stateClause, stateParams) = buildStateClause (aqStates query)
      fullQuery = "SELECT COUNT(*) FROM (" <> overviewBaseSelect <> whereClause <> ") sub" <> stateClause
  results <- queryRows conn fullQuery (params <> stateParams) :: IO [Only Int]
  pure $ maybe 0 (\(Only n) -> n) (viaNonEmpty head results)

-- | Get statistics by state and quality.
getCatalogAlbumsOverviewStats
  :: SQLite.Connection
  -> IO [(Text, Int)]  -- ^ Stats by quality
getCatalogAlbumsOverviewStats conn =
  queryRows_ conn
    "SELECT COALESCE(quality, 'unknown'), COUNT(*) \
    \FROM clusters \
    \WHERE catalog_album_id IS NOT NULL \
    \GROUP BY quality"

-- | Get all catalog albums for an artist with full overview data, sorted by
-- first release date descending (no pagination; used by the artist page).
getCatalogAlbumsByArtistOverview
  :: SQLite.Connection
  -> Int64  -- ^ Artist ID
  -> IO [CatalogAlbumOverviewRow]
getCatalogAlbumsByArtistOverview conn artistId =
  queryRows conn
    ("SELECT * FROM (" <> overviewBaseSelect <> "AND ca.artist_id = ?) sub \
     \ORDER BY first_release_date DESC")
    (Only artistId)

-- | Get album count statistics grouped by computed state, aggregated in SQL.
getCatalogAlbumsStatsByState
  :: SQLite.Connection
  -> Maybe Int64    -- ^ Optional artist ID filter
  -> Maybe Text     -- ^ Optional search query
  -> Maybe Text     -- ^ Optional release date after filter
  -> Maybe Text     -- ^ Optional release date before filter
  -> IO [(Text, Int)]  -- ^ List of (state name, count)
getCatalogAlbumsStatsByState conn maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore = do
  let (whereClause, params) = buildOverviewFilters Nothing Nothing maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore Nothing
      fullQuery =
        "SELECT computed_state, COUNT(*) FROM (" <> overviewBaseSelect <> whereClause <> ") sub \
        \GROUP BY computed_state"
  queryRows conn fullQuery params

-- * Helper functions

-- Helper function to build filter WHERE clauses
buildOverviewFilters :: Maybe Int64 -> Maybe [Text] -> Maybe Int64 -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> (Text, [SQLite.SQLData])
buildOverviewFilters maybeAlbumId maybeQualities maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore maybeReleaseGroupMBID =
  let (clauses, params) = mconcat
        [ case maybeAlbumId of
            Nothing -> ([], [])
            Just albumId -> (["AND ca.id = ?"], [toField albumId])
        , case maybeReleaseGroupMBID of
            Nothing -> ([], [])
            Just mbid -> (["AND ca.release_group_mbid = ?"], [toField mbid])
        , case maybeQualities of
            Nothing -> ([], [])
            Just qualities ->
              let placeholders = T.intercalate "," (replicate (length qualities) "?")
              in (["AND c.quality IN (" <> placeholders <> ")"], map toField qualities)
        , case maybeArtistId of
            Nothing -> ([], [])
            Just artistId -> (["AND ca.artist_id = ?"], [toField artistId])
        , case maybeSearch of
            Nothing -> ([], [])
            Just query ->
              -- Split query into words and search for each word in either title or artist
              let searchWords = T.words query
                  normalizedWords = filter (not . T.null) $ map normalizeForSearch searchWords
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

-- Helper function to build the ORDER BY clause. Everything sorts on the
-- subquery's column aliases.
buildSortClause :: Maybe Text -> Maybe Text -> Text
buildSortClause maybeSort maybeOrder =
  let order = case maybeOrder of
        Just "asc" -> " ASC"
        Just "desc" -> " DESC"
        _ -> " DESC"  -- Default to descending
      sortField = case maybeSort of
        Just "title" -> "title"
        Just "artist" -> "artist_name"
        Just "date" -> "first_release_date"
        Just "first_release_date" -> "first_release_date"
        Just "quality" -> "current_quality"
        Just "state" -> "id"
        _ -> "id"  -- Default: by ID
  in " ORDER BY " <> sortField <> order
