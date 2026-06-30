{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests for the Quality profile repository.
--
-- The interesting parts here are the JSON serialization of quality
-- preferences (which must survive a write/read cycle) and the layered
-- profile resolution: 'getEffectiveQualityProfile' walks album → artist →
-- global default, and 'resolveQualityProfileId' applies an
-- explicit → artist → source → default priority.
--
-- Note: the migrations seed several profiles and set the default, so tests
-- create their own profiles and assert against those rather than counts.
module Integration.QualityRepoSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Helpers.TestEnv (TestEnv(..), withTestEnv)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( insertQualityProfile
  , updateQualityProfile
  , deleteQualityProfile
  , getQualityProfile
  , getAllQualityProfiles
  , getEffectiveQualityProfile
  , resolveQualityProfileId
  , setDefaultQualityProfileId
  , upsertCatalogArtist
  , upsertCatalogAlbum
  , updateCatalogAlbum
  , updateCatalogArtist
  )
import Skema.Domain.Quality (QualityProfile(..), QualityPreference(..), Quality(..))
import Database.SQLite.Simple (Connection)

tests :: TestTree
tests = testGroup "Integration.QualityRepo"
  [ testCase "insert then fetch preserves fields and preferences" testRoundTrip
  , testCase "update changes the stored profile" testUpdate
  , testCase "delete removes the profile" testDelete
  , testCase "getAllQualityProfiles is ordered by name" testGetAllOrdered
  , testCase "effective profile prefers album, then artist, then default" testEffectiveCascade
  , testCase "resolveQualityProfileId honours explicit > artist > default" testResolvePriority
  ]

-- Builders ------------------------------------------------------------------

prefs :: [QualityPreference]
prefs = [QualityPreference Lossless 1 True, QualityPreference MP3_320 2 False]

mkArtist :: Connection -> Text -> IO Int64
mkArtist conn name =
  upsertCatalogArtist conn ("mbid-" <> name) name Nothing Nothing Nothing True Nothing Nothing Nothing

-- Tests ---------------------------------------------------------------------

testRoundTrip :: IO ()
testRoundTrip = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  pid <- insertQualityProfile conn "RT Profile" Lossless prefs True
  mprof <- getQualityProfile conn pid
  case mprof of
    Nothing -> assertFailure "profile not found by id"
    Just prof -> do
      qfId prof @?= Just pid
      qfName prof @?= "RT Profile"
      qfCutoffQuality prof @?= Lossless
      qfUpgradeAutomatically prof @?= True
      qfQualityPreferences prof @?= prefs  -- JSON round-trip

testUpdate :: IO ()
testUpdate = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  pid <- insertQualityProfile conn "Before" Lossless prefs True
  updateQualityProfile conn pid "After" MP3_320 [QualityPreference MP3_320 1 True] False
  mprof <- getQualityProfile conn pid
  case mprof of
    Nothing -> assertFailure "profile not found after update"
    Just prof -> do
      qfName prof @?= "After"
      qfCutoffQuality prof @?= MP3_320
      qfUpgradeAutomatically prof @?= False
      qfQualityPreferences prof @?= [QualityPreference MP3_320 1 True]

testDelete :: IO ()
testDelete = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  pid <- insertQualityProfile conn "Doomed Profile" Lossless [] False
  deleteQualityProfile conn pid
  gone <- getQualityProfile conn pid
  fmap qfId gone @?= Nothing

testGetAllOrdered :: IO ()
testGetAllOrdered = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  _ <- insertQualityProfile conn "zzz custom" Lossless [] False
  _ <- insertQualityProfile conn "aaa custom" Lossless [] False
  profiles <- getAllQualityProfiles conn
  let names = map qfName profiles
  -- ORDER BY name ASC: the full list (seeded + ours) is sorted.
  sortOn id names @?= names

testEffectiveCascade :: IO ()
testEffectiveCascade = withTestEnv $ \env -> do
  let pool = tePool env
  -- Build an artist + album and three distinct profiles.
  (albumId, artistId, albumPid, artistPid, defaultPid) <- withConnection pool $ \conn -> do
    artistId <- mkArtist conn "Cascade"
    albumId <- upsertCatalogAlbum conn "rg-cascade" "Album" artistId "mbid-Cascade" "Cascade" Nothing Nothing
    albumPid <- insertQualityProfile conn "Album Level" Lossless [] False
    artistPid <- insertQualityProfile conn "Artist Level" Lossless [] False
    defaultPid <- insertQualityProfile conn "Default Level" Lossless [] False
    setDefaultQualityProfileId conn (Just defaultPid)
    pure (albumId, artistId, albumPid, artistPid, defaultPid)

  -- No album/artist profile => falls back to the global default.
  resolvedDefault <- getEffectiveQualityProfile pool albumId
  fmap qfId resolvedDefault @?= Just (Just defaultPid)

  -- Artist profile set => used over the default.
  withConnection pool $ \conn -> updateCatalogArtist conn artistId True (Just (Just artistPid))
  resolvedArtist <- getEffectiveQualityProfile pool albumId
  fmap qfId resolvedArtist @?= Just (Just artistPid)

  -- Album profile set => wins over the artist profile.
  withConnection pool $ \conn -> updateCatalogAlbum conn albumId (Just (Just albumPid))
  resolvedAlbum <- getEffectiveQualityProfile pool albumId
  fmap qfId resolvedAlbum @?= Just (Just albumPid)

testResolvePriority :: IO ()
testResolvePriority = withTestEnv $ \env -> withConnection (tePool env) $ \conn -> do
  artistId <- mkArtist conn "Resolve"
  artistPid <- insertQualityProfile conn "Resolve Artist" Lossless [] False
  defaultPid <- insertQualityProfile conn "Resolve Default" Lossless [] False
  updateCatalogArtist conn artistId True (Just (Just artistPid))
  setDefaultQualityProfileId conn (Just defaultPid)

  -- Explicit choice always wins.
  explicit <- resolveQualityProfileId conn (Just 12345) (Just artistId) Nothing
  explicit @?= Just 12345

  -- No explicit choice, but the artist has a profile.
  viaArtist <- resolveQualityProfileId conn Nothing (Just artistId) Nothing
  viaArtist @?= Just artistPid

  -- Nothing at album/artist/source level => the global default.
  viaDefault <- resolveQualityProfileId conn Nothing Nothing Nothing
  viaDefault @?= Just defaultPid
