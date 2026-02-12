{-# LANGUAGE OverloadedStrings #-}

-- | Artist CRUD operations for catalog.
--
-- This module handles catalog_artists table operations.
module Skema.Database.Repository.Catalog.Artist
  ( -- * Artist operations
    upsertCatalogArtist
  , getCatalogArtists
  , getCatalogArtistByMBID
  , updateCatalogArtistFollowed
  , updateCatalogArtist
  , deleteCatalogArtist
    -- * Internal helpers (for other submodules)
  , normalizeForSearch
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
import qualified Data.Text.ICU.Normalize2 as ICU

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
              let searchWords = T.words query
                  normalizedWords = filter (not . T.null) $ map normalizeForSearch searchWords
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
