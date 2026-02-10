{-# LANGUAGE OverloadedStrings #-}

-- | Album CRUD operations for catalog.
--
-- This module handles catalog_albums table operations.
module Skema.Database.Repository.Catalog.Album
  ( -- * Album operations
    upsertCatalogAlbum
  , getCatalogAlbums
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
