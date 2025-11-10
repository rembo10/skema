{-# LANGUAGE OverloadedStrings #-}

-- | Acquisition repository operations (rules, tracked artists, wanted albums).
module Skema.Database.Repository.Acquisition
  ( -- * Acquisition rule operations
    createAcquisitionRule
  , getAllAcquisitionRules
  , getEnabledAcquisitionRules
  , updateAcquisitionRule
  , deleteAcquisitionRule
  , getDefaultLibraryArtistsRule
    -- * Tracked artist operations
  , insertTrackedArtist
  , getTrackedArtistByMBID
  , getAllTrackedArtists
  , getRecentTrackedArtists
  , updateTrackedArtistLastChecked
  , deleteTrackedArtist
    -- * Wanted album operations
  , insertWantedAlbum
  , getWantedAlbumByReleaseGroupMBID
  , getAllWantedAlbums
  , getWantedAlbumsByStatus
  , updateWantedAlbumStatus
  , linkWantedAlbumToCluster
  , deleteWantedAlbum
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils ()
import qualified Skema.Database.Utils as Utils
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

-- * Acquisition rule operations

-- | Create a new acquisition source (table still named acquisition_rules for backward compat).
createAcquisitionRule :: SQLite.Connection -> Text -> Maybe Text -> SourceType -> Bool -> Maybe Text -> IO Int64
createAcquisitionRule conn name description sType enabled filters = do
  results <- queryRows conn
    "INSERT INTO acquisition_rules (name, description, rule_type, enabled, filters) \
    \VALUES (?, ?, ?, ?, ?) RETURNING id"
    (name, description, Utils.sourceTypeToText sType, enabled, filters) :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only sid) -> pure sid
    Nothing -> error "Failed to get source ID after insert"

-- | Get all acquisition sources.
getAllAcquisitionRules :: SQLite.Connection -> IO [AcquisitionSourceRecord]
getAllAcquisitionRules conn =
  queryRows_ conn
    "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
    \FROM acquisition_rules ORDER BY created_at ASC"

-- | Get only enabled acquisition sources.
getEnabledAcquisitionRules :: SQLite.Connection -> IO [AcquisitionSourceRecord]
getEnabledAcquisitionRules conn =
  queryRows conn
    "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
    \FROM acquisition_rules WHERE enabled = ? ORDER BY created_at ASC"
    (Only True)

-- | Update an acquisition source.
updateAcquisitionRule :: SQLite.Connection -> Int64 -> Text -> Maybe Text -> SourceType -> Bool -> Maybe Text -> IO ()
updateAcquisitionRule conn sid name description sType enabled filters =
  executeQuery conn
    "UPDATE acquisition_rules SET name = ?, description = ?, rule_type = ?, enabled = ?, filters = ?, updated_at = CURRENT_TIMESTAMP \
    \WHERE id = ?"
    (name, description, Utils.sourceTypeToText sType, enabled, filters, sid)

-- | Delete an acquisition source.
deleteAcquisitionRule :: SQLite.Connection -> Int64 -> IO ()
deleteAcquisitionRule conn sid =
  executeQuery conn "DELETE FROM acquisition_rules WHERE id = ?" (Only sid)

-- | Get the default "library artists" source (if it exists).
getDefaultLibraryArtistsRule :: SQLite.Connection -> IO (Maybe AcquisitionSourceRecord)
getDefaultLibraryArtistsRule conn = do
  results <- queryRows conn
    "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
    \FROM acquisition_rules WHERE rule_type = ? ORDER BY created_at ASC LIMIT 1"
    (Only (Utils.sourceTypeToText LibraryArtists))
  pure $ viaNonEmpty head results

-- * Tracked artist operations

-- | Insert a new tracked artist.
insertTrackedArtist :: SQLite.Connection -> Text -> Text -> Maybe Text -> Maybe Text -> Int64 -> Maybe Int64 -> IO Int64
insertTrackedArtist conn artistMBID artistName imageUrl thumbnailUrl sid sourceClusterId = do
  results <- queryRows conn
    "INSERT INTO tracked_artists (artist_mbid, artist_name, image_url, thumbnail_url, added_by_rule_id, source_cluster_id) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(artist_mbid) DO UPDATE SET updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (artistMBID, artistName, imageUrl, thumbnailUrl, sid, sourceClusterId) :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only artistId) -> pure artistId
    Nothing -> error "Failed to get tracked artist ID after insert"

-- | Get a tracked artist by MusicBrainz ID.
getTrackedArtistByMBID :: SQLite.Connection -> Text -> IO (Maybe TrackedArtistRecord)
getTrackedArtistByMBID conn artistMBID = do
  results <- queryRows conn
    "SELECT id, artist_mbid, artist_name, image_url, thumbnail_url, added_by_rule_id, source_cluster_id, last_checked_at, created_at, updated_at \
    \FROM tracked_artists WHERE artist_mbid = ?"
    (Only artistMBID)
  pure $ viaNonEmpty head results

-- | Get all tracked artists.
getAllTrackedArtists :: SQLite.Connection -> IO [TrackedArtistRecord]
getAllTrackedArtists conn =
  queryRows_ conn
    "SELECT id, artist_mbid, artist_name, image_url, thumbnail_url, added_by_rule_id, source_cluster_id, last_checked_at, created_at, updated_at \
    \FROM tracked_artists ORDER BY artist_name ASC"

-- | Get most recently added tracked artists with a limit.
getRecentTrackedArtists :: SQLite.Connection -> Int -> IO [TrackedArtistRecord]
getRecentTrackedArtists conn limit =
  queryRows conn
    "SELECT id, artist_mbid, artist_name, image_url, thumbnail_url, added_by_rule_id, source_cluster_id, last_checked_at, created_at, updated_at \
    \FROM tracked_artists ORDER BY created_at DESC LIMIT ?"
    (Only limit)

-- | Update the last_checked_at timestamp for a tracked artist.
updateTrackedArtistLastChecked :: SQLite.Connection -> Int64 -> IO ()
updateTrackedArtistLastChecked conn artistId = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE tracked_artists SET last_checked_at = ?, updated_at = ? WHERE id = ?"
    (now, now, artistId)

-- | Delete a tracked artist.
deleteTrackedArtist :: SQLite.Connection -> Int64 -> IO ()
deleteTrackedArtist conn artistId =
  executeQuery conn "DELETE FROM tracked_artists WHERE id = ?" (Only artistId)

-- * Wanted album operations

-- | Insert a new wanted album.
insertWantedAlbum :: SQLite.Connection -> Text -> Text -> Text -> Text -> AlbumStatus -> Int64 -> Maybe Text -> IO Int64
insertWantedAlbum conn releaseGroupMBID title artistMBID artistName status sid firstReleaseDate = do
  results <- queryRows conn
    "INSERT INTO wanted_albums (release_group_mbid, title, artist_mbid, artist_name, status, added_by_rule_id, first_release_date) \
    \VALUES (?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(release_group_mbid) DO UPDATE SET updated_at = CURRENT_TIMESTAMP \
    \RETURNING id"
    (releaseGroupMBID, title, artistMBID, artistName, Utils.albumStatusToText status, sid, firstReleaseDate) :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only albumId) -> pure albumId
    Nothing -> error "Failed to get wanted album ID after insert"

-- | Get a wanted album by release group MusicBrainz ID.
getWantedAlbumByReleaseGroupMBID :: SQLite.Connection -> Text -> IO (Maybe WantedAlbumRecord)
getWantedAlbumByReleaseGroupMBID conn releaseGroupMBID = do
  results <- queryRows conn
    "SELECT id, release_group_mbid, title, artist_mbid, artist_name, status, added_by_rule_id, first_release_date, matched_cluster_id, created_at, updated_at \
    \FROM wanted_albums WHERE release_group_mbid = ?"
    (Only releaseGroupMBID)
  pure $ viaNonEmpty head results

-- | Get all wanted albums.
getAllWantedAlbums :: SQLite.Connection -> IO [WantedAlbumRecord]
getAllWantedAlbums conn =
  queryRows_ conn
    "SELECT id, release_group_mbid, title, artist_mbid, artist_name, status, added_by_rule_id, first_release_date, matched_cluster_id, created_at, updated_at \
    \FROM wanted_albums ORDER BY created_at DESC"

-- | Get wanted albums by status.
getWantedAlbumsByStatus :: SQLite.Connection -> AlbumStatus -> IO [WantedAlbumRecord]
getWantedAlbumsByStatus conn status =
  queryRows conn
    "SELECT id, release_group_mbid, title, artist_mbid, artist_name, status, added_by_rule_id, first_release_date, matched_cluster_id, created_at, updated_at \
    \FROM wanted_albums WHERE status = ? ORDER BY created_at DESC"
    (Only (Utils.albumStatusToText status))

-- | Update the status of a wanted album.
updateWantedAlbumStatus :: SQLite.Connection -> Int64 -> AlbumStatus -> IO ()
updateWantedAlbumStatus conn albumId status = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE wanted_albums SET status = ?, updated_at = ? WHERE id = ?"
    (Utils.albumStatusToText status, now, albumId)

-- | Link a wanted album to a cluster (when acquired).
linkWantedAlbumToCluster :: SQLite.Connection -> Int64 -> Int64 -> IO ()
linkWantedAlbumToCluster conn albumId cid = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE wanted_albums SET matched_cluster_id = ?, status = ?, updated_at = ? WHERE id = ?"
    (cid, Utils.albumStatusToText Acquired, now, albumId)

-- | Delete a wanted album.
deleteWantedAlbum :: SQLite.Connection -> Int64 -> IO ()
deleteWantedAlbum conn albumId =
  executeQuery conn "DELETE FROM wanted_albums WHERE id = ?" (Only albumId)
