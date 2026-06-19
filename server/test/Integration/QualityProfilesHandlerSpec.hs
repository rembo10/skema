{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Integration tests for the Quality Profiles API handlers.
--
-- These drive the real Servant handlers (auth check, DB access, response
-- construction) against a temporary database via 'runHandler', asserting on
-- both the returned values and the resulting HTTP error codes. Auth is
-- disabled under 'defaultConfig', so a 'Nothing' auth header is accepted.
--
-- This is the reference pattern for testing an API handler end-to-end
-- without standing up a WAI server.
module Integration.QualityProfilesHandlerSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Servant (Handler, NoContent, (:<|>)(..))

import Helpers.TestEnv (TestEnv(..), withTestEnv)
import Helpers.Handler (dummyJWTSecret, runOk, runStatus)

import Skema.API.Handlers.QualityProfiles (qualityProfilesServer)
import Skema.API.Types.QualityProfiles
  ( CreateQualityProfileRequest(..)
  , UpdateQualityProfileRequest(..)
  )
import Skema.Domain.Quality
  ( QualityProfile(..)
  , QualityPreference(..)
  , Quality(..)
  )
import qualified Skema.Config.Types as Cfg

tests :: TestTree
tests = testGroup "Integration.QualityProfilesHandler"
  [ testCase "create then fetch returns the stored profile" testCreateThenFetch
  , testCase "getAll includes created profiles" testGetAll
  , testCase "update modifies the profile" testUpdate
  , testCase "delete removes the profile (404 afterwards)" testDelete
  , testCase "setDefault then getDefault returns that profile" testDefault
  , testCase "fetching a missing profile returns 404" testMissing
  ]

-- Handler bundle ------------------------------------------------------------

-- | The Quality Profiles handlers, wired to a test environment.
data Handlers = Handlers
  { hGetAll :: Handler [QualityProfile]
  , hGetOne :: Int64 -> Handler QualityProfile
  , hCreate :: CreateQualityProfileRequest -> Handler QualityProfile
  , hUpdate :: Int64 -> UpdateQualityProfileRequest -> Handler QualityProfile
  , hDelete :: Int64 -> Handler NoContent
  , hGetDefault :: Handler (Maybe QualityProfile)
  , hSetDefault :: Int64 -> Handler NoContent
  }

-- | Wire the handlers to a test env (auth disabled → no auth header needed).
mkHandlers :: TestEnv -> IO Handlers
mkHandlers env = do
  jwtSecret <- dummyJWTSecret
  let server = qualityProfilesServer (Cfg.server Cfg.defaultConfig) jwtSecret (tePool env) (teConfigVar env)
      getAll :<|> getOne :<|> create :<|> update :<|> delete :<|> getDefault :<|> setDefault =
        server Nothing
  pure Handlers
    { hGetAll = getAll
    , hGetOne = getOne
    , hCreate = create
    , hUpdate = update
    , hDelete = delete
    , hGetDefault = getDefault
    , hSetDefault = setDefault
    }

-- Helpers -------------------------------------------------------------------

profileId :: QualityProfile -> IO Int64
profileId p = maybe (assertFailure "created profile has no id") pure (qfId p)

mkCreate :: Text -> CreateQualityProfileRequest
mkCreate name = CreateQualityProfileRequest
  { createQualityProfileName = name
  , createQualityProfileQualityPreferences = [QualityPreference Lossless 1 True]
  , createQualityProfileCutoffQuality = Lossless
  , createQualityProfileUpgradeAutomatically = False
  }

mkUpdate :: Text -> UpdateQualityProfileRequest
mkUpdate name = UpdateQualityProfileRequest
  { updateQualityProfileName = name
  , updateQualityProfileQualityPreferences = [QualityPreference MP3_320 2 True]
  , updateQualityProfileCutoffQuality = MP3_320
  , updateQualityProfileUpgradeAutomatically = True
  }

-- Tests ---------------------------------------------------------------------

testCreateThenFetch :: IO ()
testCreateThenFetch = withTestEnv $ \env -> do
  h <- mkHandlers env
  created <- runOk (hCreate h (mkCreate "Custom Profile A"))
  qfName created @?= "Custom Profile A"
  pid <- profileId created
  fetched <- runOk (hGetOne h pid)
  qfName fetched @?= "Custom Profile A"
  qfCutoffQuality fetched @?= Lossless

testGetAll :: IO ()
testGetAll = withTestEnv $ \env -> do
  h <- mkHandlers env
  _ <- runOk (hCreate h (mkCreate "Alpha"))
  _ <- runOk (hCreate h (mkCreate "Beta"))
  profiles <- runOk (hGetAll h)
  let names = map qfName profiles
  assertBool "Alpha present" ("Alpha" `elem` names)
  assertBool "Beta present" ("Beta" `elem` names)

testUpdate :: IO ()
testUpdate = withTestEnv $ \env -> do
  h <- mkHandlers env
  created <- runOk (hCreate h (mkCreate "Original"))
  pid <- profileId created
  _ <- runOk (hUpdate h pid (mkUpdate "Renamed"))
  fetched <- runOk (hGetOne h pid)
  qfName fetched @?= "Renamed"
  qfCutoffQuality fetched @?= MP3_320

testDelete :: IO ()
testDelete = withTestEnv $ \env -> do
  h <- mkHandlers env
  created <- runOk (hCreate h (mkCreate "Temp"))
  pid <- profileId created
  _ <- runOk (hDelete h pid)
  status <- runStatus (hGetOne h pid)
  status @?= 404

testDefault :: IO ()
testDefault = withTestEnv $ \env -> do
  h <- mkHandlers env
  created <- runOk (hCreate h (mkCreate "Default Me"))
  pid <- profileId created
  _ <- runOk (hSetDefault h pid)
  def <- runOk (hGetDefault h)
  fmap qfId def @?= Just (Just pid)

testMissing :: IO ()
testMissing = withTestEnv $ \env -> do
  h <- mkHandlers env
  status <- runStatus (hGetOne h 999999)
  status @?= 404
