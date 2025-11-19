{-# LANGUAGE OverloadedStrings #-}

-- | Cluster repository operations.
module Skema.Database.Repository.Clusters
  ( -- * Cluster operations
    computeClusterHash
  , findClusterByHash
  , createCluster
  , updateClusterWithMBData
  , updateClusterLastIdentified
  , updateTrackCluster
  , getClusterById
  , getAllClusters
  , getClusterWithTracks
  , emptyMetadataRecord
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import Skema.Database.Repository.Tracks (stringToOsPath)
import Skema.MusicBrainz.Types (MBRelease(..), MBID(..), unMBID)
import System.OsPath (OsPath)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Aeson (encode)
import qualified Crypto.Hash as Hash

-- * Cluster operations

-- | Compute a stable hash for a cluster based on identifying metadata.
-- This hash is used to determine if a cluster needs re-identification.
computeClusterHash :: Maybe Text -> Maybe Text -> Int -> Text
computeClusterHash album albumArtist trackCount =
  let
    -- Normalize text: lowercase and trim
    normalize :: Maybe Text -> Text
    normalize = maybe "" (T.toLower . T.strip)

    -- Combine identifying fields
    combined = normalize album <> "|" <> normalize albumArtist <> "|" <> show trackCount

    -- Compute SHA256 hash
    hash :: Hash.Digest Hash.SHA256
    hash = Hash.hash (encodeUtf8 combined :: ByteString)

    -- Convert to hex text
  in show hash

-- | Find an existing cluster by its metadata hash.
findClusterByHash :: SQLite.Connection -> Text -> IO (Maybe ClusterRecord)
findClusterByHash conn hash = do
  results <- queryRows conn
    "SELECT id, metadata_hash, album, album_artist, track_count, mb_release_id, mb_release_group_id, mb_confidence, created_at, updated_at, last_identified_at, mb_release_data, mb_candidates \
    \FROM clusters WHERE metadata_hash = ?"
    (Only hash)
  pure $ viaNonEmpty head results

-- | Create a new cluster.
createCluster :: SQLite.Connection -> Text -> Maybe Text -> Maybe Text -> Int -> IO Int64
createCluster conn hash album albumArtist trackCount =
  insertReturningId conn
    "INSERT INTO clusters (metadata_hash, album, album_artist, track_count) VALUES (?, ?, ?, ?) RETURNING id"
    (hash, album, albumArtist, trackCount)

-- | Update a cluster with MusicBrainz match data.
-- Now accepts the full MBRelease and candidate list, caching both as JSON for performance.
updateClusterWithMBData :: SQLite.Connection -> Int64 -> MBRelease -> Double -> [MBRelease] -> IO ()
updateClusterWithMBData conn cid release confidence candidates = do
  now <- getCurrentTime
  let releaseId = unMBID (mbReleaseId release)
      releaseGroupId = fmap unMBID (mbReleaseGroupId release)
      -- Serialize the full MBRelease to JSON for caching
      releaseJson = TL.toStrict $ TLE.decodeUtf8 $ encode release
      -- Serialize candidates list to JSON for user selection
      candidatesJson = if null candidates
                       then Nothing
                       else Just $ TL.toStrict $ TLE.decodeUtf8 $ encode candidates
  executeQuery conn
    "UPDATE clusters SET mb_release_id = ?, mb_release_group_id = ?, mb_confidence = ?, last_identified_at = ?, updated_at = ?, mb_release_data = ?, mb_candidates = ? WHERE id = ?"
    (Just releaseId, releaseGroupId, Just confidence, Just now, now, Just releaseJson, candidatesJson, cid)

-- | Update last_identified_at timestamp for a cluster (even when no match found).
-- This prevents repeatedly trying to identify clusters that failed to match.
updateClusterLastIdentified :: SQLite.Connection -> Int64 -> IO ()
updateClusterLastIdentified conn cid = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE clusters SET last_identified_at = ?, updated_at = ? WHERE id = ?"
    (Just now, now, cid)

-- | Update the cluster_id for tracks.
-- Replaces the old junction table approach with direct FK assignment.
updateTrackCluster :: SQLite.Connection -> Int64 -> [Int64] -> IO ()
updateTrackCluster conn cid trackIds = do
  forM_ trackIds $ \tid ->
    executeQuery conn
      "UPDATE library_tracks SET cluster_id = ? WHERE id = ?"
      (cid, tid)

-- | Get a cluster by ID.
getClusterById :: SQLite.Connection -> Int64 -> IO (Maybe ClusterRecord)
getClusterById conn cid = do
  results <- queryRows conn
    "SELECT id, metadata_hash, album, album_artist, track_count, mb_release_id, mb_release_group_id, mb_confidence, created_at, updated_at, last_identified_at, mb_release_data, mb_candidates \
    \FROM clusters WHERE id = ?"
    (Only cid)
  pure $ viaNonEmpty head results

-- | Get all clusters, ordered by most recently updated first.
getAllClusters :: SQLite.Connection -> IO [ClusterRecord]
getAllClusters conn =
  queryRows_ conn
    "SELECT id, metadata_hash, album, album_artist, track_count, mb_release_id, mb_release_group_id, mb_confidence, created_at, updated_at, last_identified_at, mb_release_data, mb_candidates \
    \FROM clusters ORDER BY updated_at DESC"

-- | Create an empty metadata record for a track.
emptyMetadataRecord :: Int64 -> LibraryTrackMetadataRecord
emptyMetadataRecord tid = LibraryTrackMetadataRecord
  { metaTrackId = Just tid
  , metaFormat = Nothing
  , metaTitle = Nothing
  , metaArtist = Nothing
  , metaAlbum = Nothing
  , metaAlbumArtist = Nothing
  , metaTrackNumber = Nothing
  , metaTotalTracks = Nothing
  , metaDiscNumber = Nothing
  , metaTotalDiscs = Nothing
  , metaDate = Nothing
  , metaYear = Nothing
  , metaGenre = Nothing
  , metaPublisher = Nothing
  , metaComment = Nothing
  , metaDurationSeconds = Nothing
  , metaBitsPerSample = Nothing
  , metaCountry = Nothing
  , metaLabel = Nothing
  , metaCatalogNumber = Nothing
  , metaBarcode = Nothing
  , metaReleaseStatus = Nothing
  , metaReleaseType = Nothing
  , metaMBRecordingId = Nothing
  , metaMBTrackId = Nothing
  , metaMBReleaseId = Nothing
  , metaMBReleaseGroupId = Nothing
  , metaMBArtistId = Nothing
  , metaMBAlbumArtistId = Nothing
  , metaMBWorkId = Nothing
  , metaMBDiscId = Nothing
  , metaAcoustidFingerprint = Nothing
  , metaAcoustidId = Nothing
  , metaCreatedAt = Nothing
  , metaUpdatedAt = Nothing
  }

-- | Get a cluster with all its tracks and metadata.
getClusterWithTracks :: SQLite.Connection -> Int64 -> IO (Maybe (ClusterRecord, [(Int64, OsPath, LibraryTrackMetadataRecord)]))
getClusterWithTracks conn cid = do
  -- Get the cluster
  maybeCluster <- getClusterById conn cid
  case maybeCluster of
    Nothing -> pure Nothing
    Just cluster -> do
      -- Get all tracks in the cluster
      trackIds <- queryRows conn
        "SELECT id, path FROM library_tracks WHERE cluster_id = ? ORDER BY id"
        (Only cid) :: IO [(Int64, String)]

      -- For each track, get its metadata
      tracks <- forM trackIds $ \(tid, pathStr) -> do
        path <- stringToOsPath pathStr
        -- Get metadata using the function from Repository.Tracks
        results <- queryRows conn
          "SELECT track_id, format, title, artist, album, album_artist, track_number, total_tracks, disc_number, total_discs, date, year, genre, publisher, comment, duration_seconds, bits_per_sample, country, label, catalog_number, barcode, release_status, release_type, mb_recording_id, mb_track_id, mb_release_id, mb_release_group_id, mb_artist_id, mb_album_artist_id, mb_work_id, mb_disc_id, acoustid_fingerprint, acoustid_id, created_at, updated_at \
          \FROM library_track_metadata WHERE track_id = ?"
          (Only tid)
        let maybeMeta = viaNonEmpty head results
        let metadata = fromMaybe (emptyMetadataRecord tid) maybeMeta
        pure (tid, path, metadata)

      pure $ Just (cluster, tracks)
