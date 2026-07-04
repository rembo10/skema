{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Integration tests for the Acquisition service's 'handleCatalogAlbumAdded'.
--
-- Focus is the "auto upgrade existing albums" branch: when the
-- @auto_upgrade_existing_albums@ library setting is on and a discovered album
-- already has a cluster on disk, the handler assigns the default quality
-- profile (enrolling it for monitoring) and emits 'WantedAlbumAdded' so the
-- download service can search for an upgrade. Crucially this bypasses the
-- acquisition release-status filter (default "upcoming only"), which would
-- otherwise exclude an old back-catalog album (one with no future release
-- date), leaving it unmonitored.
--
-- These run the handler directly against a real temp database with the config
-- TVar overwritten per case, and assert on both the persisted profile and the
-- emitted event.
module Integration.AcquisitionServiceSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Concurrent.STM as STM

import Helpers.TestEnv (TestEnv(..), withTestEnv)
import Helpers.EventAssertions (waitForEventWithTimeout)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( upsertCatalogArtist
  , upsertCatalogAlbum
  , createCluster
  , computeClusterHash
  , getCatalogAlbumById
  , getDefaultQualityProfileId
  )
import qualified Skema.Database.Types as DB
import Skema.Events.Bus (subscribe)
import Skema.Events.Types (Event(..))
import Skema.Config.Types (Config(..), LibraryConfig(..))
import Skema.Services.Dependencies (AcquisitionDeps(..))
import Skema.Services.Acquisition (handleCatalogAlbumAdded)
import qualified Database.SQLite.Simple as SQLite

tests :: TestTree
tests = testGroup "Integration.AcquisitionService"
  [ testCase "flag on + album on disk assigns default profile and wants an upgrade" testEnrollsExistingAlbum
  , testCase "flag off leaves an on-disk album unassigned" testFlagOffLeavesAlone
  , testCase "flag on but album not on disk is not enrolled" testNotInLibraryNotEnrolled
  ]

-- Fixtures ------------------------------------------------------------------

artistMbid :: Text
artistMbid = "mbid-artist"

rgMbid :: Text
rgMbid = "rg-existing"

albumTitle :: Text
albumTitle = "Back Catalogue"

-- | Build acquisition deps from a test env.
mkDeps :: TestEnv -> AcquisitionDeps
mkDeps env = AcquisitionDeps
  { acqEventBus = teEventBus env
  , acqLogEnv = teLogEnv env
  , acqDbPool = tePool env
  , acqConfigVar = teConfigVar env
  , acqMBClient = teMBClientEnv env
  , acqClock = teClock env
  }

-- | Flip the @auto_upgrade_existing_albums@ library setting.
setAutoUpgradeExisting :: TestEnv -> Bool -> IO ()
setAutoUpgradeExisting env enabled =
  STM.atomically $ STM.modifyTVar' (teConfigVar env) $ \c ->
    c { library = (library c) { libraryAutoUpgradeExistingAlbums = enabled } }

-- | Seed a followed artist and an album with no release date (so it is never
-- "upcoming" and would be rejected by the default acquisition filter). No
-- quality profile is assigned. Returns the album id.
seedFollowedAlbum :: SQLite.Connection -> IO Int64
seedFollowedAlbum conn = do
  artistId <- upsertCatalogArtist conn artistMbid "Artist"
    Nothing Nothing Nothing True Nothing Nothing Nothing
  upsertCatalogAlbum conn rgMbid albumTitle artistId artistMbid "Artist" Nothing Nothing

-- | Link a cluster on disk to the album, marking it "in library".
linkCluster :: SQLite.Connection -> Int64 -> IO ()
linkCluster conn albumId = do
  cid <- createCluster conn (computeClusterHash (Just albumTitle) (Just "Artist") 8) (Just albumTitle) (Just "Artist") 8
  SQLite.execute conn "UPDATE clusters SET catalog_album_id = ? WHERE id = ?" (albumId, cid)

isWantedAlbumAdded :: Event -> Bool
isWantedAlbumAdded = \case
  WantedAlbumAdded{} -> True
  _ -> False

profileIdOf :: TestEnv -> Int64 -> IO (Maybe Int64)
profileIdOf env albumId = withConnection (tePool env) $ \conn ->
  (DB.catalogAlbumQualityProfileId =<<) <$> getCatalogAlbumById conn albumId

-- Tests ---------------------------------------------------------------------

testEnrollsExistingAlbum :: IO ()
testEnrollsExistingAlbum = withTestEnv $ \env -> do
  albumId <- withConnection (tePool env) $ \conn -> do
    aid <- seedFollowedAlbum conn
    linkCluster conn aid
    pure aid
  setAutoUpgradeExisting env True

  eventChan <- STM.atomically $ subscribe (teEventBus env)
  handleCatalogAlbumAdded (mkDeps env) rgMbid albumTitle artistMbid albumId

  -- The default quality profile is now assigned (album is monitored).
  expected <- withConnection (tePool env) getDefaultQualityProfileId
  assigned <- profileIdOf env albumId
  assertBool "a default quality profile is seeded" (isJust expected)
  assigned @?= expected

  -- And an upgrade search was requested.
  wanted <- waitForEventWithTimeout eventChan isWantedAlbumAdded (500 * 1000)
  assertBool "WantedAlbumAdded should be emitted for the enrolled album" (isJust wanted)

testFlagOffLeavesAlone :: IO ()
testFlagOffLeavesAlone = withTestEnv $ \env -> do
  albumId <- withConnection (tePool env) $ \conn -> do
    aid <- seedFollowedAlbum conn
    linkCluster conn aid
    pure aid
  -- Flag defaults to off; do not enable it.

  eventChan <- STM.atomically $ subscribe (teEventBus env)
  handleCatalogAlbumAdded (mkDeps env) rgMbid albumTitle artistMbid albumId

  -- No acquisition rules and not upcoming: the album stays unassigned.
  assigned <- profileIdOf env albumId
  assigned @?= Nothing

  wanted <- waitForEventWithTimeout eventChan isWantedAlbumAdded (200 * 1000)
  assertBool "no WantedAlbumAdded when the flag is off" (isNothing wanted)

testNotInLibraryNotEnrolled :: IO ()
testNotInLibraryNotEnrolled = withTestEnv $ \env -> do
  -- Album is followed but has no cluster on disk.
  albumId <- withConnection (tePool env) seedFollowedAlbum
  setAutoUpgradeExisting env True

  eventChan <- STM.atomically $ subscribe (teEventBus env)
  handleCatalogAlbumAdded (mkDeps env) rgMbid albumTitle artistMbid albumId

  -- Not in the library, so the auto-upgrade branch does not fire; with no
  -- acquisition rules and no future release date it stays unassigned.
  assigned <- profileIdOf env albumId
  assigned @?= Nothing

  wanted <- waitForEventWithTimeout eventChan isWantedAlbumAdded (200 * 1000)
  assertBool "no WantedAlbumAdded for an album not in the library" (isNothing wanted)
