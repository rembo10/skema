{-# LANGUAGE OverloadedStrings #-}

-- | Library statistics repository operations.
module Skema.Database.Repository.Stats
  ( getLibraryStats
  ) where

import Skema.Database.Connection
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

-- | Get library statistics.
getLibraryStats :: SQLite.Connection -> IO (Int, Int, Int, Int, Int, Double, Int, Integer, Double)
getLibraryStats conn = do
  -- Total tracks
  totalTracksResult <- queryRows_ conn "SELECT COUNT(*) FROM library_tracks" :: IO [Only Int]
  let totalTracks = case viaNonEmpty head totalTracksResult of
        Just (Only n) -> n
        Nothing -> 0

  -- Total albums (distinct album values)
  totalAlbumsResult <- queryRows_ conn
    "SELECT COUNT(DISTINCT album) FROM library_track_metadata WHERE album IS NOT NULL" :: IO [Only Int]
  let totalAlbums = case viaNonEmpty head totalAlbumsResult of
        Just (Only n) -> n
        Nothing -> 0

  -- Total artists (distinct artist values)
  totalArtistsResult <- queryRows_ conn
    "SELECT COUNT(DISTINCT COALESCE(album_artist, artist)) FROM library_track_metadata WHERE artist IS NOT NULL OR album_artist IS NOT NULL" :: IO [Only Int]
  let totalArtists = case viaNonEmpty head totalArtistsResult of
        Just (Only n) -> n
        Nothing -> 0

  -- Matched tracks (tracks in clusters with MusicBrainz IDs)
  matchedTracksResult <- queryRows_ conn
    "SELECT COUNT(*) FROM library_tracks t \
    \JOIN clusters c ON t.cluster_id = c.id \
    \WHERE c.mb_release_id IS NOT NULL" :: IO [Only Int]
  let matchedTracks = case viaNonEmpty head matchedTracksResult of
        Just (Only n) -> n
        Nothing -> 0

  let unmatchedTracks = totalTracks - matchedTracks

  -- Total diffs
  totalDiffsResult <- queryRows_ conn "SELECT COUNT(*) FROM metadata_diffs" :: IO [Only Int]
  let totalDiffs = case viaNonEmpty head totalDiffsResult of
        Just (Only n) -> n
        Nothing -> 0

  -- Total library size (sum of track sizes)
  totalSizeResult <- queryRows_ conn "SELECT COALESCE(SUM(size), 0) FROM library_tracks" :: IO [Only Integer]
  let totalSize = case viaNonEmpty head totalSizeResult of
        Just (Only n) -> n
        Nothing -> 0

  -- Total runtime (sum of durations)
  totalRuntimeResult <- queryRows_ conn "SELECT COALESCE(SUM(duration_seconds), 0.0) FROM library_track_metadata" :: IO [Only Double]
  let totalRuntime = case viaNonEmpty head totalRuntimeResult of
        Just (Only n) -> n
        Nothing -> 0.0

  -- Metadata accuracy percentage
  -- We check 12 fields: title, track_number, track_artist, album, album_artist, date, year, country, label, catalog_number, barcode, genre
  let fieldsChecked = 12
  let totalFieldChecks = matchedTracks * fieldsChecked
  let accuracy = if totalFieldChecks > 0
        then (fromIntegral (totalFieldChecks - totalDiffs) / fromIntegral totalFieldChecks) * 100.0
        else 0.0

  pure (totalTracks, totalAlbums, totalArtists, matchedTracks, unmatchedTracks, accuracy, totalDiffs, totalSize, totalRuntime)
