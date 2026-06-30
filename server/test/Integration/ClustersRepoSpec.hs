{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Clusters repository.
--
-- Covers the pure 'computeClusterHash' (determinism and normalisation, since
-- it gates re-identification), the create/find-by-hash round-trip, the
-- MusicBrainz match update and its inverse 'clearClusterRelease', and
-- 'computeClusterQuality'/'updateClusterQuality', which take the lowest
-- track quality as the album's representative quality.
module Integration.ClustersRepoSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

import Helpers.TestEnv (TestEnv(..), withTestEnv)
import Helpers.Builders (mkTestMBRelease)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( computeClusterHash
  , createCluster
  , findClusterByHash
  , getClusterById
  , updateClusterWithMBData
  , clearClusterRelease
  , computeClusterQuality
  , updateClusterQuality
  , upsertCatalogArtist
  , upsertCatalogAlbum
  )
import Skema.Database.Types (ClusterRecord(..))
import Skema.Domain.Quality (Quality(..), qualityToText)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite

tests :: TestTree
tests = testGroup "Integration.ClustersRepo"
  [ testGroup "computeClusterHash"
    [ testCase "is deterministic for the same inputs" $
        computeClusterHash (Just "Album") (Just "Artist") 10
          @?= computeClusterHash (Just "Album") (Just "Artist") 10

    , testCase "normalises case and surrounding whitespace" $
        computeClusterHash (Just "  Album  ") (Just "ARTIST") 10
          @?= computeClusterHash (Just "album") (Just "artist") 10

    , testCase "changes with the track count" $
        assertBool "different track counts should differ" $
          computeClusterHash (Just "Album") (Just "Artist") 10
            /= computeClusterHash (Just "Album") (Just "Artist") 11

    , testCase "does not collide when album and artist are swapped" $
        assertBool "album/artist fields are kept distinct" $
          computeClusterHash (Just "A") (Just "B") 1
            /= computeClusterHash (Just "B") (Just "A") 1
    ]

  , testCase "create then find by hash round-trips" testCreateFind
  , testCase "findClusterByHash returns Nothing for an unknown hash" testFindMiss
  , testCase "MB match update sets release data, clear wipes it" testMBUpdateClear
  , testCase "cluster quality is the lowest track quality" testClusterQuality
  ]

-- Builders ------------------------------------------------------------------

day :: Integer -> Int -> Int -> UTCTime
day y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

-- | Insert a library_track row carrying a quality, linked to a cluster.
insertTrackRow :: Connection -> Text -> Int64 -> Text -> IO ()
insertTrackRow conn path cid quality =
  SQLite.execute conn
    "INSERT INTO library_tracks (path, size, modified_at, cluster_id, quality) VALUES (?, ?, ?, ?, ?)"
    (path, 1000 :: Integer, day 2025 1 1, cid, quality)

-- Tests ---------------------------------------------------------------------

testCreateFind :: IO ()
testCreateFind = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  let hash = computeClusterHash (Just "Album") (Just "Artist") 12
  cid <- createCluster conn hash (Just "Album") (Just "Artist") 12

  found <- findClusterByHash conn hash
  case found of
    Nothing -> assertFailure "cluster not found by hash"
    Just rec -> do
      clusterId rec @?= Just cid
      clusterMetadataHash rec @?= hash
      clusterAlbum rec @?= Just "Album"
      clusterAlbumArtist rec @?= Just "Artist"
      clusterTrackCount rec @?= 12

  byId <- getClusterById conn cid
  fmap clusterMetadataHash byId @?= Just hash

testFindMiss :: IO ()
testFindMiss = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  found <- findClusterByHash conn "nonexistent-hash"
  fmap clusterId found @?= Nothing

testMBUpdateClear :: IO ()
testMBUpdateClear = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  -- A catalog album for the release group so the FK gets resolved on update.
  artistId <- upsertCatalogArtist conn "mbid-art" "Artist" Nothing Nothing Nothing True Nothing Nothing Nothing
  _ <- upsertCatalogAlbum conn "rel-1-rg" "Album" artistId "mbid-art" "Artist" Nothing Nothing

  cid <- createCluster conn (computeClusterHash (Just "Album") (Just "Artist") 10) (Just "Album") (Just "Artist") 10
  let release = mkTestMBRelease "rel-1" "Album" "Artist" "mbid-art" 10

  updateClusterWithMBData conn cid release 0.95 []
  afterUpdate <- getClusterById conn cid
  case afterUpdate of
    Nothing -> assertFailure "cluster vanished after MB update"
    Just rec -> do
      clusterMBReleaseId rec @?= Just "rel-1"
      clusterMBReleaseGroupId rec @?= Just "rel-1-rg"
      clusterMBConfidence rec @?= Just 0.95

  clearClusterRelease conn cid True
  afterClear <- getClusterById conn cid
  case afterClear of
    Nothing -> assertFailure "cluster vanished after clear"
    Just rec -> do
      clusterMBReleaseId rec @?= Nothing
      clusterMBReleaseGroupId rec @?= Nothing
      clusterMBConfidence rec @?= Nothing

testClusterQuality :: IO ()
testClusterQuality = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  cid <- createCluster conn (computeClusterHash (Just "Album") (Just "Artist") 2) (Just "Album") (Just "Artist") 2
  insertTrackRow conn "/lib/a.flac" cid (qualityToText Lossless)
  insertTrackRow conn "/lib/b.mp3" cid (qualityToText MP3_320)

  -- The album's representative quality is the worst track quality.
  computed <- computeClusterQuality conn cid
  computed @?= Just MP3_320

  -- updateClusterQuality persists it to the cluster row.
  updateClusterQuality conn cid
  rec <- getClusterById conn cid
  fmap clusterQuality rec @?= Just (Just (qualityToText MP3_320))
