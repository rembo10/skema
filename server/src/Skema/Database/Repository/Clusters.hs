{-# LANGUAGE OverloadedStrings #-}

-- | Cluster repository operations.
module Skema.Database.Repository.Clusters
  ( -- * Cluster operations
    computeClusterHash
  , findClusterByHash
  , createCluster
  , updateClusterWithMBData
  , updateClusterWithMBDataManual
  , updateClusterLastIdentified
  , updateClusterWithCandidates
  , updateTrackCluster
  , getClusterById
  , getAllClusters
  , getClusterWithTracks
  , emptyMetadataRecord
  , computeClusterQuality
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import Skema.Database.Repository.Tracks (stringToOsPath)
import Skema.MusicBrainz.Types (MBRelease(..), MBID(..), unMBID)
import Skema.Services.Common (metadataRecordToMonatone)
import Skema.Domain.Quality (Quality(..), detectQualityFromAudio, qualityToText, textToQuality)
import System.OsPath (OsPath)
import Data.Time (getCurrentTime)
import Data.List (minimum)
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
    "SELECT id, metadata_hash, album, album_artist, track_count, mb_release_id, mb_release_group_id, mb_confidence, created_at, updated_at, last_identified_at, mb_release_data, mb_candidates, match_source, match_locked \
    \FROM clusters WHERE metadata_hash = ?"
    (Only hash)
  pure $ viaNonEmpty head results

-- | Create a new cluster.
createCluster :: SQLite.Connection -> Text -> Maybe Text -> Maybe Text -> Int -> IO Int64
createCluster conn hash album albumArtist trackCount =
  insertReturningId conn
    "INSERT INTO clusters (metadata_hash, album, album_artist, track_count) VALUES (?, ?, ?, ?) RETURNING id"
    (hash, album, albumArtist, trackCount)

-- | Update a cluster with MusicBrainz match data (automatic matching).
-- Now accepts the full MBRelease and candidate list, caching both as JSON for performance.
-- Sets match_source to auto_metadata and does NOT lock the match.
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
      -- Auto matching source
      matchSource = matchSourceToText AutoMetadata
  executeQuery conn
    "UPDATE clusters SET mb_release_id = ?, mb_release_group_id = ?, mb_confidence = ?, last_identified_at = ?, updated_at = ?, mb_release_data = ?, mb_candidates = ?, match_source = ? WHERE id = ?"
    (Just releaseId, releaseGroupId, Just confidence, Just now, now, Just releaseJson, candidatesJson, Just matchSource, cid)

-- | Update a cluster with MusicBrainz match data (manual assignment).
-- This version marks the match as "manual" and LOCKS it to prevent automatic re-matching.
updateClusterWithMBDataManual :: SQLite.Connection -> Int64 -> MBRelease -> Double -> IO ()
updateClusterWithMBDataManual conn cid release confidence = do
  now <- getCurrentTime
  let releaseId = unMBID (mbReleaseId release)
      releaseGroupId = fmap unMBID (mbReleaseGroupId release)
      -- Serialize the full MBRelease to JSON for caching
      releaseJson = TL.toStrict $ TLE.decodeUtf8 $ encode release
      -- Manual matching source
      matchSource = matchSourceToText Manual
  executeQuery conn
    "UPDATE clusters SET mb_release_id = ?, mb_release_group_id = ?, mb_confidence = ?, last_identified_at = ?, updated_at = ?, mb_release_data = ?, match_source = ?, match_locked = ? WHERE id = ?"
    (Just releaseId, releaseGroupId, Just confidence, Just now, now, Just releaseJson, Just matchSource, True, cid)

-- | Update last_identified_at timestamp for a cluster (even when no match found).
-- This prevents repeatedly trying to identify clusters that failed to match.
updateClusterLastIdentified :: SQLite.Connection -> Int64 -> IO ()
updateClusterLastIdentified conn cid = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE clusters SET last_identified_at = ?, updated_at = ? WHERE id = ?"
    (Just now, now, cid)

-- | Update a cluster with candidates but no match (confidence below threshold).
-- Saves the candidates for manual selection while marking that identification was attempted.
updateClusterWithCandidates :: SQLite.Connection -> Int64 -> [MBRelease] -> IO ()
updateClusterWithCandidates conn cid candidates = do
  now <- getCurrentTime
  let candidatesJson = if null candidates
                       then Nothing
                       else Just $ TL.toStrict $ TLE.decodeUtf8 $ encode candidates
  executeQuery conn
    "UPDATE clusters SET mb_candidates = ?, last_identified_at = ?, updated_at = ? WHERE id = ?"
    (candidatesJson, Just now, now, cid)

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
    "SELECT id, metadata_hash, album, album_artist, track_count, mb_release_id, mb_release_group_id, mb_confidence, created_at, updated_at, last_identified_at, mb_release_data, mb_candidates, match_source, match_locked \
    \FROM clusters WHERE id = ?"
    (Only cid)
  pure $ viaNonEmpty head results

-- | Get all clusters, ordered by most recently updated first.
getAllClusters :: SQLite.Connection -> IO [ClusterRecord]
getAllClusters conn =
  queryRows_ conn
    "SELECT id, metadata_hash, album, album_artist, track_count, mb_release_id, mb_release_group_id, mb_confidence, created_at, updated_at, last_identified_at, mb_release_data, mb_candidates, match_source, match_locked \
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
-- Returns tuples of (track_id, path, metadata, mb_recording_id, mb_recording_title)
getClusterWithTracks :: SQLite.Connection -> Int64 -> IO (Maybe (ClusterRecord, [(Int64, OsPath, LibraryTrackMetadataRecord, Maybe Text, Maybe Text)]))
getClusterWithTracks conn cid = do
  -- Get the cluster
  maybeCluster <- getClusterById conn cid
  case maybeCluster of
    Nothing -> pure Nothing
    Just cluster -> do
      -- Get all tracks in the cluster with recording IDs
      trackData <- queryRows conn
        "SELECT id, path, mb_recording_id, mb_recording_title FROM library_tracks WHERE cluster_id = ? ORDER BY id"
        (Only cid) :: IO [(Int64, String, Maybe Text, Maybe Text)]

      -- For each track, get its metadata
      tracks <- forM trackData $ \(tid, pathStr, mbRecId, mbRecTitle) -> do
        path <- stringToOsPath pathStr
        -- Get metadata using the function from Repository.Tracks
        results <- queryRows conn
          "SELECT track_id, format, title, artist, album, album_artist, track_number, total_tracks, disc_number, total_discs, date, year, genre, publisher, comment, duration_seconds, bits_per_sample, country, label, catalog_number, barcode, release_status, release_type, mb_recording_id, mb_track_id, mb_release_id, mb_release_group_id, mb_artist_id, mb_album_artist_id, mb_work_id, mb_disc_id, acoustid_fingerprint, acoustid_id, created_at, updated_at \
          \FROM library_track_metadata WHERE track_id = ?"
          (Only tid)
        let maybeMeta = viaNonEmpty head results
        let metadata = fromMaybe (emptyMetadataRecord tid) maybeMeta
        pure (tid, path, metadata, mbRecId, mbRecTitle)

      pure $ Just (cluster, tracks)

-- | Compute the quality of a cluster from its tracks' stored quality values.
-- Returns the lowest quality found across all tracks (representative of the album).
computeClusterQuality :: SQLite.Connection -> Int64 -> IO (Maybe Quality)
computeClusterQuality conn clusterId = do
  -- Read quality values directly from library_tracks table
  qualityTexts <- queryRows conn
    "SELECT quality FROM library_tracks WHERE cluster_id = ? AND quality IS NOT NULL"
    (Only clusterId) :: IO [Only (Maybe Text)]

  let qualities = mapMaybe (\(Only maybeQText) -> maybeQText >>= textToQuality) qualityTexts

  -- Take the minimum quality (worst quality in the album)
  -- Quality has Ord instance where Unknown < MP3_192 < ... < HiResLossless
  case viaNonEmpty minimum qualities of
    Nothing -> pure Nothing
    Just quality -> pure $ Just quality
