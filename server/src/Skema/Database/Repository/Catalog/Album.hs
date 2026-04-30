{-# LANGUAGE OverloadedStrings #-}

-- | Album CRUD operations for catalog.
--
-- This module handles catalog_albums table operations.
module Skema.Database.Repository.Catalog.Album
  ( -- * Album operations
    upsertCatalogAlbum
  , getCatalogAlbums
  , getCatalogAlbumById
  , getCatalogAlbumsByArtistId
  , getCatalogAlbumByReleaseGroupMBID
  , updateCatalogAlbum
  , deleteCatalogAlbum
  ) where

import Skema.Database.Repository.Catalog.Artist (normalizeForSearch)

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

-- * Catalog album operations

-- | Columns selected for a 'CatalogAlbumRecord'. The current_quality value is
-- derived from the latest cluster linked via clusters.catalog_album_id rather
-- than stored on the album row itself.
albumSelectColumns :: Text
albumSelectColumns =
  "ca.id, ca.release_group_mbid, ca.title, ca.artist_id, ca.artist_mbid, ca.artist_name, \
  \ca.album_type, ca.first_release_date, ca.album_cover_url, ca.album_cover_thumbnail_url, \
  \ca.quality_profile_id, \
  \(SELECT c.quality FROM clusters c WHERE c.catalog_album_id = ca.id ORDER BY c.id DESC LIMIT 1) AS current_quality, \
  \ca.created_at, ca.updated_at"

-- | Upsert a catalog album (insert or update if exists). After upsert, link
-- any existing unlinked clusters whose mb_release_group_id matches the album's
-- release group, so previously-orphaned library imports become discoverable.
upsertCatalogAlbum :: SQLite.Connection -> Text -> Text -> Int64 -> Text -> Text -> Maybe Text -> Maybe Text -> IO Int64
upsertCatalogAlbum conn releaseGroupMBID title artistId artistMBID artistName albumType firstReleaseDate = do
  let normalizedTitle = normalizeForSearch title
      normalizedArtistName = normalizeForSearch artistName
  albumId <- insertReturningId conn
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
  executeQuery conn
    "UPDATE clusters SET catalog_album_id = ? \
    \WHERE catalog_album_id IS NULL AND mb_release_group_id = ?"
    (albumId, releaseGroupMBID)
  pure albumId

-- | Get all catalog albums.
getCatalogAlbums :: SQLite.Connection -> IO [CatalogAlbumRecord]
getCatalogAlbums conn =
  queryRows_ conn ("SELECT " <> albumSelectColumns <> " FROM catalog_albums ca ORDER BY ca.created_at DESC")

-- | Get a catalog album by database ID.
getCatalogAlbumById :: SQLite.Connection -> Int64 -> IO (Maybe CatalogAlbumRecord)
getCatalogAlbumById conn albumId = do
  results <- queryRows conn
    ("SELECT " <> albumSelectColumns <> " FROM catalog_albums ca WHERE ca.id = ?")
    (Only albumId)
  pure $ viaNonEmpty head results

-- | Get catalog albums by internal artist ID.
getCatalogAlbumsByArtistId :: SQLite.Connection -> Int64 -> IO [CatalogAlbumRecord]
getCatalogAlbumsByArtistId conn artistId =
  queryRows conn
    ("SELECT " <> albumSelectColumns <> " FROM catalog_albums ca WHERE ca.artist_id = ? ORDER BY ca.first_release_date DESC")
    (Only artistId)

-- | Get a catalog album by release group MusicBrainz ID.
getCatalogAlbumByReleaseGroupMBID :: SQLite.Connection -> Text -> IO (Maybe CatalogAlbumRecord)
getCatalogAlbumByReleaseGroupMBID conn releaseGroupMBID = do
  results <- queryRows conn
    ("SELECT " <> albumSelectColumns <> " FROM catalog_albums ca WHERE ca.release_group_mbid = ?")
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
