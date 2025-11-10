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
  , updateCatalogAlbumWanted
  , updateCatalogAlbum
  , deleteCatalogAlbum
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

-- * Catalog artist operations

-- | Upsert a catalog artist (insert or update if exists).
upsertCatalogArtist :: SQLite.Connection -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Maybe Int64 -> Maybe Int64 -> Maybe UTCTime -> IO Int64
upsertCatalogArtist conn artistMBID artistName artistType imageUrl thumbnailUrl followed addedByRuleId sourceClusterId lastCheckedAt = do
  results <- queryRows conn
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
    (artistMBID, artistName, artistType, imageUrl, thumbnailUrl, followed, addedByRuleId, sourceClusterId, lastCheckedAt) :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only artistId) -> pure artistId
    Nothing -> error "Failed to get catalog artist ID after upsert"

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
upsertCatalogAlbum :: SQLite.Connection -> Text -> Text -> Int64 -> Text -> Text -> Maybe Text -> Maybe Text -> Bool -> Maybe Int64 -> IO Int64
upsertCatalogAlbum conn releaseGroupMBID title artistId artistMBID artistName albumType firstReleaseDate wanted matchedClusterId = do
  results <- queryRows conn
    "INSERT INTO catalog_albums (release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, wanted, matched_cluster_id) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(release_group_mbid) DO UPDATE SET \
    \  title = excluded.title, \
    \  artist_id = excluded.artist_id, \
    \  artist_mbid = excluded.artist_mbid, \
    \  artist_name = excluded.artist_name, \
    \  album_type = excluded.album_type, \
    \  first_release_date = excluded.first_release_date, \
    \  wanted = excluded.wanted, \
    \  matched_cluster_id = excluded.matched_cluster_id, \
    \  updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (releaseGroupMBID, title, artistId, artistMBID, artistName, albumType, firstReleaseDate, wanted, matchedClusterId) :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only albumId) -> pure albumId
    Nothing -> error "Failed to get catalog album ID after upsert"

-- | Get catalog albums, optionally filtered by wanted status.
-- Use getCatalogAlbumsByArtistId to filter by artist.
getCatalogAlbums :: SQLite.Connection -> Maybe Bool -> IO [CatalogAlbumRecord]
getCatalogAlbums conn maybeWanted =
  case maybeWanted of
    Nothing -> queryRows_ conn
      "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, wanted, user_unwanted, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
      \FROM catalog_albums ORDER BY created_at DESC"
    Just wanted -> queryRows conn
      "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, wanted, user_unwanted, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
      \FROM catalog_albums WHERE wanted = ? ORDER BY created_at DESC"
      (Only wanted)

-- | Get catalog albums by internal artist ID, optionally filtered by wanted status.
getCatalogAlbumsByArtistId :: SQLite.Connection -> Int64 -> Maybe Bool -> IO [CatalogAlbumRecord]
getCatalogAlbumsByArtistId conn artistId maybeWanted =
  case maybeWanted of
    Nothing -> queryRows conn
      "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, wanted, user_unwanted, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
      \FROM catalog_albums WHERE artist_id = ? ORDER BY first_release_date DESC"
      (Only artistId)
    Just wanted -> queryRows conn
      "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, wanted, user_unwanted, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
      \FROM catalog_albums WHERE artist_id = ? AND wanted = ? ORDER BY first_release_date DESC"
      (artistId, wanted)

-- | Get a catalog album by release group MusicBrainz ID.
getCatalogAlbumByReleaseGroupMBID :: SQLite.Connection -> Text -> IO (Maybe CatalogAlbumRecord)
getCatalogAlbumByReleaseGroupMBID conn releaseGroupMBID = do
  results <- queryRows conn
    "SELECT id, release_group_mbid, title, artist_id, artist_mbid, artist_name, album_type, first_release_date, album_cover_url, album_cover_thumbnail_url, wanted, user_unwanted, matched_cluster_id, quality_profile_id, current_quality, created_at, updated_at \
    \FROM catalog_albums WHERE release_group_mbid = ?"
    (Only releaseGroupMBID)
  pure $ viaNonEmpty head results

-- | Update the wanted status of a catalog album.
-- When marking as wanted (True), clears user_unwanted flag.
-- When marking as unwanted (False), sets user_unwanted flag to prevent auto-wanting.
updateCatalogAlbumWanted :: SQLite.Connection -> Int64 -> Bool -> IO ()
updateCatalogAlbumWanted conn albumId wanted = do
  now <- getCurrentTime
  let userUnwanted = not wanted  -- If unwanted, set user_unwanted=true
  executeQuery conn
    "UPDATE catalog_albums SET wanted = ?, user_unwanted = ?, updated_at = ? WHERE id = ?"
    (wanted, userUnwanted, now, albumId)

-- | Update a catalog album (wanted status and quality profile).
updateCatalogAlbum :: SQLite.Connection -> Int64 -> Bool -> Maybe (Maybe Int64) -> IO ()
updateCatalogAlbum conn albumId wanted maybeProfileId = do
  now <- getCurrentTime
  let userUnwanted = not wanted
  case maybeProfileId of
    Nothing ->
      -- No change to quality_profile_id
      executeQuery conn
        "UPDATE catalog_albums SET wanted = ?, user_unwanted = ?, updated_at = ? WHERE id = ?"
        (wanted, userUnwanted, now, albumId)
    Just Nothing ->
      -- Clear quality_profile_id
      executeQuery conn
        "UPDATE catalog_albums SET wanted = ?, user_unwanted = ?, quality_profile_id = NULL, updated_at = ? WHERE id = ?"
        (wanted, userUnwanted, now, albumId)
    Just (Just profileId) ->
      -- Set quality_profile_id
      executeQuery conn
        "UPDATE catalog_albums SET wanted = ?, user_unwanted = ?, quality_profile_id = ?, updated_at = ? WHERE id = ?"
        (wanted, userUnwanted, profileId, now, albumId)

-- | Delete a catalog album.
deleteCatalogAlbum :: SQLite.Connection -> Int64 -> IO ()
deleteCatalogAlbum conn albumId =
  executeQuery conn "DELETE FROM catalog_albums WHERE id = ?" (Only albumId)
