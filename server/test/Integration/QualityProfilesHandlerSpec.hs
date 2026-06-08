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

import qualified Data.Text as T
import Servant (Handler, NoContent, ServerError(..), (:<|>)(..))
import Servant.Server (runHandler)

import Helpers.TestEnv (TestEnv(..), withTestEnv)

import Skema.API.Handlers.QualityProfiles (qualityProfilesServer)
import Skema.API.Types.QualityProfiles
  ( CreateQualityProfileRequest(..)
  , UpdateQualityProfileRequest(..)
  )
import Skema.Auth.JWT (JWTSecret, getJWTSecret)
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

-- | Build a real 'JWTSecret'. Its value is never inspected because auth is
-- disabled in tests, but the handler signature requires one.
dummyJWTSecret :: IO JWTSecret
dummyJWTSecret = do
  let cfg = (Cfg.server Cfg.defaultConfig) { Cfg.serverJwtSecret = Just (T.replicate 64 "0") }
  maybe (assertFailure "could not build test JWT secret") pure =<< getJWTSecret cfg

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

-- | Run a handler expecting success.
runOk :: Handler a -> IO a
runOk h = either (assertFailure . ("unexpected error response: " <>) . show) pure =<< runHandler h

-- | Run a handler expecting a failure, returning the HTTP status code.
runStatus :: Handler a -> IO Int
runStatus h = do
  result <- runHandler h
  case result of
    Left err -> pure (errHTTPCode err)
    Right _ -> assertFailure "expected an error response, got success"

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
