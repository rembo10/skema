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
import Data.Char (isAlphaNum)
import qualified Data.Text.ICU.Normalize as ICU

-- * Helper functions

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

-- * Catalog artist operations

-- | Upsert a catalog artist (insert or update if exists).
upsertCatalogArtist :: SQLite.Connection -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Maybe Int64 -> Maybe Int64 -> Maybe UTCTime -> IO Int64
upsertCatalogArtist conn artistMBID artistName artistType imageUrl thumbnailUrl followed addedByRuleId sourceClusterId lastCheckedAt =
  let normalizedName = normalizeForSearch artistName
  in insertReturningId conn
    "INSERT INTO catalog_artists (artist_mbid, artist_name, artist_name_normalized, artist_type, image_url, thumbnail_url, followed, added_by_rule_id, source_cluster_id, last_checked_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(artist_mbid) DO UPDATE SET \
    \  artist_name = excluded.artist_name, \
    \  artist_name_normalized = excluded.artist_name_normalized, \
    \  artist_type = excluded.artist_type, \
    \  image_url = COALESCE(excluded.image_url, catalog_artists.image_url), \
    \  thumbnail_url = COALESCE(excluded.thumbnail_url, catalog_artists.thumbnail_url), \
    \  followed = excluded.followed, \
    \  added_by_rule_id = COALESCE(excluded.added_by_rule_id, catalog_artists.added_by_rule_id), \
    \  source_cluster_id = COALESCE(excluded.source_cluster_id, catalog_artists.source_cluster_id), \
    \  last_checked_at = COALESCE(excluded.last_checked_at, catalog_artists.last_checked_at), \
    \  updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (artistMBID, artistName, normalizedName, artistType, imageUrl, thumbnailUrl, followed, addedByRuleId, sourceClusterId, lastCheckedAt)

-- | Get catalog artists, optionally filtered by followed status, with search and sorting.
getCatalogArtists :: SQLite.Connection -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> IO [CatalogArtistRecord]
getCatalogArtists conn maybeFollowed maybeSearch maybeSort maybeOrder = do
  -- For completion sorting, we need a more complex query with subqueries
  let needsCompletionCalc = maybeSort == Just "completion"

  if needsCompletionCalc
    then do
      -- Complex query with completion calculation
      let query = "SELECT ca.id, ca.artist_mbid, ca.artist_name, ca.artist_type, ca.image_url, ca.thumbnail_url, \
                  \ca.followed, ca.added_by_rule_id, ca.source_cluster_id, ca.last_checked_at, ca.quality_profile_id, \
                  \ca.created_at, ca.updated_at \
                  \FROM catalog_artists ca \
                  \LEFT JOIN ( \
                  \  SELECT artist_id, \
                  \    COUNT(*) as total_albums, \
                  \    SUM(CASE WHEN current_quality IS NOT NULL THEN 1 ELSE 0 END) as albums_in_library \
                  \  FROM catalog_albums \
                  \  GROUP BY artist_id \
                  \) album_stats ON ca.id = album_stats.artist_id \
                  \WHERE 1=1 "

      let (whereClause, params) = buildArtistFilters maybeFollowed maybeSearch

      let orderClause = case maybeOrder of
            Just "asc" -> " ORDER BY CAST(COALESCE(album_stats.albums_in_library, 0) AS REAL) / NULLIF(COALESCE(album_stats.total_albums, 1), 0) ASC"
            _ -> " ORDER BY CAST(COALESCE(album_stats.albums_in_library, 0) AS REAL) / NULLIF(COALESCE(album_stats.total_albums, 1), 0) DESC"

      queryRows conn (query <> whereClause <> orderClause) params
    else do
      -- Simple query without completion calculation
      let baseQuery = "SELECT id, artist_mbid, artist_name, artist_type, image_url, thumbnail_url, followed, added_by_rule_id, source_cluster_id, last_checked_at, quality_profile_id, created_at, updated_at \
                      \FROM catalog_artists WHERE 1=1 "

      -- Build WHERE clause
      let (whereClause, params) = buildArtistFilters maybeFollowed maybeSearch

      -- Build ORDER BY clause
      let orderClause = buildArtistSortClause maybeSort maybeOrder

      let fullQuery = baseQuery <> whereClause <> orderClause

      queryRows conn fullQuery params

-- Helper function to build artist filter WHERE clauses
buildArtistFilters :: Maybe Bool -> Maybe Text -> (Text, [SQLite.SQLData])
buildArtistFilters maybeFollowed maybeSearch =
  let (clauses, params) = mconcat
        [ case maybeFollowed of
            Nothing -> ([], [])
            Just followed -> (["AND followed = ?"], [toField followed])
        , case maybeSearch of
            Nothing -> ([], [])
            Just query ->
              -- Split query into words and search for each word independently
              let words = T.words query
                  normalizedWords = filter (not . T.null) $ map normalizeForSearch words
                  wordClauses = map (\_ -> "artist_name_normalized LIKE ?") normalizedWords
                  wordParams = map (\w -> toField ("%" <> w <> "%")) normalizedWords
              in if null normalizedWords
                   then ([], [])
                   else (["AND (" <> T.intercalate " AND " wordClauses <> ")"], wordParams)
        ]
  in (T.unwords clauses, params)

-- Helper function to build artist ORDER BY clause
buildArtistSortClause :: Maybe Text -> Maybe Text -> Text
buildArtistSortClause maybeSort maybeOrder =
  let order = case maybeOrder of
        Just "asc" -> " ASC"
        Just "desc" -> " DESC"
        _ -> " DESC"  -- Default to descending
      sortField = case maybeSort of
        Just "name" -> "artist_name"
        Just "date_added" -> "created_at"
        _ -> "created_at"  -- Default: by creation date
  in " ORDER BY " <> sortField <> order

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
upsertCatalogAlbum :: SQLite.Connection -> Text -> Text -> Int64 -> Text -> Text -> Maybe Text -> Maybe Text -> IO Int64
upsertCatalogAlbum conn releaseGroupMBID title artistId artistMBID artistName albumType firstReleaseDate =
  let normalizedTitle = normalizeForSearch title
      normalizedArtistName = normalizeForSearch artistName
  in insertReturningId conn
    "INSERT INTO catalog_albums (release_group_mbid, title, title_normalized, artist_id, artist_mbid, artist_name, artist_name_normalized, album_type, first_release_date) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(release_group_mbid) DO UPDATE SET \
    \  title = excluded.title, \
    \  title_normalized = excluded.title_normalized, \
    \  artist_id = excluded.artist_id, \
    \  artist_mbid = excluded.artist_mbid, \
    \  artist_name = excluded.artist_name, \
    \  artist_name_normalized = excluded.artist_name_normalized, \
    \  album_type = excluded.album_type, \
    \  first_release_date = excluded.first_release_date, \
    \  updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (releaseGroupMBID, title, normalizedTitle, artistId, artistMBID, artistName, normalizedArtistName, albumType, firstReleaseDate)

-- | Get all catalog albums.
getCatalogAlbums :: SQLite.Connection -> IO [CatalogAlbumRecord]
getCatalogAlbums conn =
  queryRows_ conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums ORDER BY created_at DESC"

-- | Get catalog albums by internal artist ID.
getCatalogAlbumsByArtistId :: SQLite.Connection -> Int64 -> IO [CatalogAlbumRecord]
getCatalogAlbumsByArtistId conn artistId =
  queryRows conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums WHERE artist_id = ? ORDER BY first_release_date DESC"
    (Only artistId)

-- | Get a catalog album by release group MusicBrainz ID.
getCatalogAlbumByReleaseGroupMBID :: SQLite.Connection -> Text -> IO (Maybe CatalogAlbumRecord)
getCatalogAlbumByReleaseGroupMBID conn releaseGroupMBID = do
  results <- queryRows conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, quality_profile_id, current_quality, created_at, updated_at \
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
  -> Maybe [Text]  -- ^ Filter by state
  -> Maybe [Text]  -- ^ Filter by quality
  -> Maybe Int64   -- ^ Filter by artist ID
  -> Maybe Text    -- ^ Search query
  -> Maybe Text    -- ^ Sort field
  -> Maybe Text    -- ^ Sort order (asc/desc)
  -> Maybe Text    -- ^ Release date after (inclusive, e.g., "2025-01-01" or "today")
  -> Maybe Text    -- ^ Release date before (inclusive, e.g., "2025-12-31" or "today")
  -> IO [CatalogAlbumOverviewRow]
getCatalogAlbumsOverview conn limit offset maybeStates maybeQualities maybeArtistId maybeSearch maybeSort maybeOrder maybeReleaseDateAfter maybeReleaseDateBefore = do
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
        \  AND d_active.status IN ('queued', 'downloading', 'processing') \
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
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'identification_failed' THEN 'IdentificationFailed' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NULL THEN 'Monitored' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NOT NULL THEN 'Upgrading' \
                         \    ELSE 'Wanted' \
                         \  END AS computed_state \
                         \FROM catalog_albums ca \
                         \LEFT JOIN quality_profiles qp ON ca.quality_profile_id = qp.id \
                         \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
                         \  AND d_active.status IN ('queued', 'downloading', 'processing') \
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
  -> Maybe [Text]  -- ^ Filter by state
  -> Maybe [Text]  -- ^ Filter by quality
  -> Maybe Int64   -- ^ Filter by artist ID
  -> Maybe Text    -- ^ Search query
  -> Maybe Text    -- ^ Release date after
  -> Maybe Text    -- ^ Release date before
  -> IO Int
getCatalogAlbumsOverviewCount conn maybeStates maybeQualities maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore = do
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
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NULL AND d_active.id IS NOT NULL AND d_active.status = 'identification_failed' THEN 'IdentificationFailed' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NULL THEN 'Monitored' \
                         \    WHEN ca.quality_profile_id IS NOT NULL AND c.id IS NOT NULL AND d_active.id IS NOT NULL THEN 'Upgrading' \
                         \    ELSE 'Wanted' \
                         \  END AS computed_state \
                         \FROM catalog_albums ca \
                         \LEFT JOIN downloads d_active ON ca.id = d_active.catalog_album_id \
                         \  AND d_active.status IN ('queued', 'downloading', 'processing') \
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
