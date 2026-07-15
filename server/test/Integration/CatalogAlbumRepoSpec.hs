{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests for the catalog Album repository.
--
-- Beyond plain CRUD, these pin two behaviours that are easy to break:
-- the @ON CONFLICT(release_group_mbid)@ upsert (updates in place rather
-- than duplicating, and links any orphaned cluster sharing the release
-- group), and the @current_quality@ column, which is not stored on the
-- album row but derived from the latest linked cluster via the
-- @clusters.catalog_album_id@ FK.
module Integration.CatalogAlbumRepoSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.TestEnv (TestEnv(..), withTestEnv)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( upsertCatalogArtist
  , upsertCatalogAlbum
  , getCatalogAlbums
  , getCatalogAlbumById
  , getCatalogAlbumsByArtistId
  , getCatalogAlbumByReleaseGroupMBID
  , updateCatalogAlbum
  , deleteCatalogAlbum
  , createCluster
  , insertQualityProfile
  , getCatalogAlbumsOverview
  , CatalogAlbumOverviewRow(..)
  , AlbumQuery(..)
  , defaultAlbumQuery
  )
import Skema.Database.Types (CatalogAlbumRecord(..))
import Skema.Domain.Quality (Quality(..), qualityToText)
import Database.SQLite.Simple (Connection, Only(..))
import qualified Database.SQLite.Simple as SQLite

tests :: TestTree
tests = testGroup "Integration.CatalogAlbumRepo"
  [ testCase "upsert insert then fetch preserves fields" testUpsertInsert
  , testCase "upsert on existing release group updates in place" testUpsertConflict
  , testCase "getByReleaseGroupMBID finds the album, or Nothing" testGetByRG
  , testCase "getByArtistId filters and orders by release date" testGetByArtist
  , testCase "updateCatalogAlbum sets, clears, and preserves the profile" testUpdateProfile
  , testCase "deleteCatalogAlbum removes the row" testDelete
  , testCase "upsert links an orphaned cluster sharing the release group" testUpsertLinksCluster
  , testCase "current_quality is derived from the linked cluster" testCurrentQualityDerived
  , testCase "overview wanted reflects cluster quality vs profile cutoff" testOverviewWanted
  ]

-- Builders ------------------------------------------------------------------

-- | Create a catalog artist, returning its id.
mkArtist :: Connection -> Text -> IO Int64
mkArtist conn name =
  upsertCatalogArtist conn ("mbid-" <> name) name Nothing Nothing Nothing True Nothing Nothing Nothing

-- Tests ---------------------------------------------------------------------

testUpsertInsert :: IO ()
testUpsertInsert = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  albumId <- upsertCatalogAlbum conn "rg-1" "First Album" artistId "mbid-Artist" "Artist" (Just "Album") (Just "2024-05-01")

  mrec <- getCatalogAlbumById conn albumId
  case mrec of
    Nothing -> assertFailure "album not found by id"
    Just rec -> do
      catalogAlbumId rec @?= Just albumId
      catalogAlbumReleaseGroupMBID rec @?= "rg-1"
      catalogAlbumTitle rec @?= "First Album"
      catalogAlbumArtistId rec @?= Just artistId
      catalogAlbumArtistName rec @?= "Artist"
      catalogAlbumType rec @?= Just "Album"
      catalogAlbumFirstReleaseDate rec @?= Just "2024-05-01"
      catalogAlbumQualityProfileId rec @?= Nothing
      catalogAlbumCurrentQuality rec @?= Nothing

testUpsertConflict :: IO ()
testUpsertConflict = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  firstId <- upsertCatalogAlbum conn "rg-dup" "Original Title" artistId "mbid-Artist" "Artist" (Just "Album") Nothing
  secondId <- upsertCatalogAlbum conn "rg-dup" "Updated Title" artistId "mbid-Artist" "Artist" (Just "EP") Nothing

  -- Same release group => same row updated, not a duplicate.
  secondId @?= firstId
  albums <- getCatalogAlbums conn
  length albums @?= 1

  mrec <- getCatalogAlbumById conn firstId
  fmap catalogAlbumTitle mrec @?= Just "Updated Title"
  fmap catalogAlbumType mrec @?= Just (Just "EP")

testGetByRG :: IO ()
testGetByRG = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  albumId <- upsertCatalogAlbum conn "rg-find" "Findable" artistId "mbid-Artist" "Artist" Nothing Nothing

  found <- getCatalogAlbumByReleaseGroupMBID conn "rg-find"
  fmap catalogAlbumId found @?= Just (Just albumId)

  missing <- getCatalogAlbumByReleaseGroupMBID conn "rg-missing"
  fmap catalogAlbumId missing @?= Nothing

testGetByArtist :: IO ()
testGetByArtist = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Main"
  otherId <- mkArtist conn "Other"

  _ <- upsertCatalogAlbum conn "rg-old" "Older" artistId "mbid-Main" "Main" Nothing (Just "2020-01-01")
  _ <- upsertCatalogAlbum conn "rg-new" "Newer" artistId "mbid-Main" "Main" Nothing (Just "2023-01-01")
  _ <- upsertCatalogAlbum conn "rg-other" "Other Album" otherId "mbid-Other" "Other" Nothing (Just "2024-01-01")

  albums <- getCatalogAlbumsByArtistId conn artistId
  -- Only this artist's albums, newest release date first.
  map catalogAlbumTitle albums @?= ["Newer", "Older"]

testUpdateProfile :: IO ()
testUpdateProfile = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  albumId <- upsertCatalogAlbum conn "rg-prof" "Album" artistId "mbid-Artist" "Artist" Nothing Nothing
  profileId <- insertQualityProfile conn "Custom Profile" Lossless [] False

  -- Set the profile.
  updateCatalogAlbum conn albumId (Just (Just profileId))
  afterSet <- getCatalogAlbumById conn albumId
  fmap catalogAlbumQualityProfileId afterSet @?= Just (Just profileId)

  -- A 'Nothing' update preserves the existing profile (timestamp-only).
  updateCatalogAlbum conn albumId Nothing
  afterNoop <- getCatalogAlbumById conn albumId
  fmap catalogAlbumQualityProfileId afterNoop @?= Just (Just profileId)

  -- 'Just Nothing' clears the profile.
  updateCatalogAlbum conn albumId (Just Nothing)
  afterClear <- getCatalogAlbumById conn albumId
  fmap catalogAlbumQualityProfileId afterClear @?= Just Nothing

testDelete :: IO ()
testDelete = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  albumId <- upsertCatalogAlbum conn "rg-del" "Doomed" artistId "mbid-Artist" "Artist" Nothing Nothing

  deleteCatalogAlbum conn albumId
  gone <- getCatalogAlbumById conn albumId
  fmap catalogAlbumId gone @?= Nothing

testUpsertLinksCluster :: IO ()
testUpsertLinksCluster = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"

  -- An imported cluster exists for this release group but isn't linked yet.
  clusterId <- createCluster conn "hash-1" (Just "Album") (Just "Artist") 10
  SQLite.execute conn "UPDATE clusters SET mb_release_group_id = ? WHERE id = ?"
    ("rg-link" :: Text, clusterId)

  -- Upserting the album should adopt the orphaned cluster.
  albumId <- upsertCatalogAlbum conn "rg-link" "Album" artistId "mbid-Artist" "Artist" Nothing Nothing

  linked <- clusterCatalogAlbumId conn clusterId
  linked @?= Just albumId

testCurrentQualityDerived :: IO ()
testCurrentQualityDerived = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  albumId <- upsertCatalogAlbum conn "rg-q" "Album" artistId "mbid-Artist" "Artist" Nothing Nothing

  -- Before any linked cluster, current_quality is absent.
  beforeLink <- getCatalogAlbumById conn albumId
  (catalogAlbumCurrentQuality =<< beforeLink) @?= Nothing

  -- Link a cluster carrying a quality; the album derives it via the FK.
  clusterId <- createCluster conn "hash-q" (Just "Album") (Just "Artist") 10
  SQLite.execute conn "UPDATE clusters SET quality = ?, catalog_album_id = ? WHERE id = ?"
    ("Lossless" :: Text, albumId, clusterId)

  afterLink <- getCatalogAlbumById conn albumId
  (catalogAlbumCurrentQuality =<< afterLink) @?= Just "Lossless"

testOverviewWanted :: IO ()
testOverviewWanted = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Artist"
  albumId <- upsertCatalogAlbum conn "rg-wanted" "Album" artistId "mbid-Artist" "Artist" Nothing Nothing
  profileId <- insertQualityProfile conn "Lossless Profile" Lossless [] False
  updateCatalogAlbum conn albumId (Just (Just profileId))

  -- No cluster on disk yet: wanted.
  noCluster <- overviewWanted conn albumId
  noCluster @?= Just True

  -- A cluster at cutoff quality satisfies the profile: not wanted.
  clusterId <- createCluster conn "hash-wanted" (Just "Album") (Just "Artist") 10
  SQLite.execute conn "UPDATE clusters SET quality = ?, catalog_album_id = ? WHERE id = ?"
    (qualityToText Lossless, albumId, clusterId)
  atCutoff <- overviewWanted conn albumId
  atCutoff @?= Just False

  -- Above-cutoff quality also satisfies it.
  SQLite.execute conn "UPDATE clusters SET quality = ? WHERE id = ?"
    (qualityToText HiResLossless, clusterId)
  aboveCutoff <- overviewWanted conn albumId
  aboveCutoff @?= Just False

  -- Below-cutoff quality still wants an upgrade.
  SQLite.execute conn "UPDATE clusters SET quality = ? WHERE id = ?"
    (qualityToText MP3_320, clusterId)
  belowCutoff <- overviewWanted conn albumId
  belowCutoff @?= Just True

overviewWanted :: Connection -> Int64 -> IO (Maybe Bool)
overviewWanted conn albumId = do
  rows <- getCatalogAlbumsOverview conn defaultAlbumQuery { aqAlbumId = Just albumId }
  pure $ caorWanted <$> viaNonEmpty head rows

-- Helpers -------------------------------------------------------------------

clusterCatalogAlbumId :: Connection -> Int64 -> IO (Maybe Int64)
clusterCatalogAlbumId conn cid = do
  rows <- SQLite.query conn
    "SELECT catalog_album_id FROM clusters WHERE id = ?" (Only cid)
    :: IO [Only (Maybe Int64)]
  pure $ case rows of
    (Only v : _) -> v
    [] -> Nothing
