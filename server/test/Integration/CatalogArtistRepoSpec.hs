{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests for the catalog Artist repository.
--
-- The behaviours worth pinning beyond plain CRUD are the
-- @ON CONFLICT(artist_mbid)@ upsert, which COALESCEs image/thumbnail (a
-- re-upsert with Nothing must not wipe an existing image), the three-way
-- 'updateCatalogArtist' profile update, and 'updateCatalogArtistName',
-- which also rewrites the denormalised artist_name on the artist's albums.
module Integration.CatalogArtistRepoSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.TestEnv (TestEnv(..), withTestEnv)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( upsertCatalogArtist
  , getCatalogArtistById
  , getCatalogArtistByMBID
  , updateCatalogArtistFollowed
  , updateCatalogArtist
  , updateCatalogArtistName
  , deleteCatalogArtist
  , insertQualityProfile
  , upsertCatalogAlbum
  , getCatalogAlbumById
  )
import Skema.Database.Types (CatalogArtistRecord(..), CatalogAlbumRecord(..))
import Skema.Domain.Quality (Quality(..))
import Database.SQLite.Simple (Connection)

tests :: TestTree
tests = testGroup "Integration.CatalogArtistRepo"
  [ testCase "upsert insert then fetch preserves fields" testUpsertInsert
  , testCase "upsert on conflict updates name but coalesces image" testUpsertCoalesce
  , testCase "getByMBID finds the artist, or Nothing" testGetByMBID
  , testCase "updateCatalogArtistFollowed toggles followed" testFollowed
  , testCase "updateCatalogArtist sets, clears, and preserves the profile" testUpdateProfile
  , testCase "updateCatalogArtistName cascades to the artist's albums" testRenameCascade
  , testCase "deleteCatalogArtist removes the row" testDelete
  ]

-- Builders ------------------------------------------------------------------

-- | Insert an artist with explicit mbid/name/image, returning its id.
insertArtist :: Connection -> Text -> Text -> Maybe Text -> IO Int64
insertArtist conn mbid name image =
  upsertCatalogArtist conn mbid name Nothing image Nothing True Nothing Nothing Nothing

-- Tests ---------------------------------------------------------------------

testUpsertInsert :: IO ()
testUpsertInsert = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-a" "The Artist" (Just "http://img/a.jpg")
  mrec <- getCatalogArtistById conn artistId
  case mrec of
    Nothing -> assertFailure "artist not found by id"
    Just rec -> do
      catalogArtistId rec @?= Just artistId
      catalogArtistMBID rec @?= "mbid-a"
      catalogArtistName rec @?= "The Artist"
      catalogArtistImageUrl rec @?= Just "http://img/a.jpg"
      catalogArtistFollowed rec @?= True

testUpsertCoalesce :: IO ()
testUpsertCoalesce = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-c" "Original Name" (Just "http://img/original.jpg")
  -- Re-upsert with a new name but no image; the image must be preserved.
  secondId <- insertArtist conn "mbid-c" "Renamed" Nothing
  secondId @?= artistId

  mrec <- getCatalogArtistById conn artistId
  case mrec of
    Nothing -> assertFailure "artist not found after re-upsert"
    Just rec -> do
      catalogArtistName rec @?= "Renamed"
      catalogArtistImageUrl rec @?= Just "http://img/original.jpg"  -- COALESCEd

testGetByMBID :: IO ()
testGetByMBID = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-find" "Findable" Nothing
  found <- getCatalogArtistByMBID conn "mbid-find"
  fmap catalogArtistId found @?= Just (Just artistId)
  missing <- getCatalogArtistByMBID conn "mbid-nope"
  fmap catalogArtistId missing @?= Nothing

testFollowed :: IO ()
testFollowed = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-f" "Artist" Nothing
  updateCatalogArtistFollowed conn artistId False
  unfollowed <- getCatalogArtistById conn artistId
  fmap catalogArtistFollowed unfollowed @?= Just False
  updateCatalogArtistFollowed conn artistId True
  refollowed <- getCatalogArtistById conn artistId
  fmap catalogArtistFollowed refollowed @?= Just True

testUpdateProfile :: IO ()
testUpdateProfile = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-p" "Artist" Nothing
  pid <- insertQualityProfile conn "Artist Custom Profile" Lossless [] False

  updateCatalogArtist conn artistId True (Just (Just pid))
  afterSet <- getCatalogArtistById conn artistId
  fmap catalogArtistQualityProfileId afterSet @?= Just (Just pid)

  -- A 'Nothing' update leaves the profile untouched.
  updateCatalogArtist conn artistId True Nothing
  afterNoop <- getCatalogArtistById conn artistId
  fmap catalogArtistQualityProfileId afterNoop @?= Just (Just pid)

  -- 'Just Nothing' clears it.
  updateCatalogArtist conn artistId True (Just Nothing)
  afterClear <- getCatalogArtistById conn artistId
  fmap catalogArtistQualityProfileId afterClear @?= Just Nothing

testRenameCascade :: IO ()
testRenameCascade = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-rename" "Old Name" Nothing
  albumId <- upsertCatalogAlbum conn "rg-rename" "Album" artistId "mbid-rename" "Old Name" Nothing Nothing

  updateCatalogArtistName conn artistId "New Name"

  -- The artist row is renamed...
  artist <- getCatalogArtistById conn artistId
  fmap catalogArtistName artist @?= Just "New Name"
  -- ...and the denormalised artist_name on the album follows.
  album <- getCatalogAlbumById conn albumId
  fmap catalogAlbumArtistName album @?= Just "New Name"

testDelete :: IO ()
testDelete = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- insertArtist conn "mbid-del" "Doomed" Nothing
  deleteCatalogArtist conn artistId
  gone <- getCatalogArtistById conn artistId
  fmap catalogArtistId gone @?= Nothing
