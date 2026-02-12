{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the config system.
--
-- Tests:
-- - Environment variable overrides
-- - Config to API JSON conversion (computed fields)
-- - JSON merge-based config updates
-- - Schema generation
module Unit.ConfigSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import System.Environment (setEnv, unsetEnv)

import Skema.Config.Types
import Skema.Config.EnvOverrides (applyEnvOverrides, fieldToEnvVar, fieldToEnvVars)
import Skema.Config.Schema (schemaToJSON, allSchemas)
import Skema.Domain.ConfigJSON (configToAPIJSON, applyConfigJSONUpdate)

tests :: TestTree
tests = testGroup "Unit.Config"
  [ envOverrideTests
  , fieldToEnvVarTests
  , configToAPIJSONTests
  , configUpdateTests
  , schemaTests
  ]

-- =============================================================================
-- Environment Variable Override Tests
-- =============================================================================

envOverrideTests :: TestTree
envOverrideTests = testGroup "Env Var Overrides"
  [ testCase "SKEMA_SERVER_PORT overrides server port" $ do
      let cfg = defaultConfig
      -- Set env var
      setEnv "SKEMA_SERVER_PORT" "9999"

      -- Apply overrides
      updatedCfg <- applyEnvOverrides cfg

      -- Clean up
      unsetEnv "SKEMA_SERVER_PORT"

      -- Verify
      serverPort (server updatedCfg) @?= 9999

  , testCase "SKEMA_SERVER_HOST overrides server host" $ do
      let cfg = defaultConfig
      setEnv "SKEMA_SERVER_HOST" "0.0.0.0"

      updatedCfg <- applyEnvOverrides cfg

      unsetEnv "SKEMA_SERVER_HOST"

      serverHost (server updatedCfg) @?= "0.0.0.0"

  , testCase "SKEMA_LIBRARY_WATCH overrides library watch setting" $ do
      let cfg = defaultConfig { library = (library defaultConfig) { libraryWatch = True } }
      setEnv "SKEMA_LIBRARY_WATCH" "false"

      updatedCfg <- applyEnvOverrides cfg

      unsetEnv "SKEMA_LIBRARY_WATCH"

      libraryWatch (library updatedCfg) @?= False

  , testCase "Unset env vars don't affect config" $ do
      let cfg = defaultConfig
      -- Ensure env var is not set
      unsetEnv "SKEMA_SERVER_PORT"

      updatedCfg <- applyEnvOverrides cfg

      -- Port should still be default
      serverPort (server updatedCfg) @?= 8182

  , testCase "SKEMA_PORT (short form) overrides server port" $ do
      let cfg = defaultConfig
      -- Ensure full form is not set
      unsetEnv "SKEMA_SERVER_PORT"
      setEnv "SKEMA_PORT" "7777"

      updatedCfg <- applyEnvOverrides cfg

      unsetEnv "SKEMA_PORT"

      serverPort (server updatedCfg) @?= 7777

  , testCase "SKEMA_HOST (short form) overrides server host" $ do
      let cfg = defaultConfig
      unsetEnv "SKEMA_SERVER_HOST"
      setEnv "SKEMA_HOST" "192.168.1.1"

      updatedCfg <- applyEnvOverrides cfg

      unsetEnv "SKEMA_HOST"

      serverHost (server updatedCfg) @?= "192.168.1.1"

  , testCase "Full form takes precedence over short form" $ do
      let cfg = defaultConfig
      -- Set both forms - full should win
      setEnv "SKEMA_SERVER_PORT" "9999"
      setEnv "SKEMA_PORT" "7777"

      updatedCfg <- applyEnvOverrides cfg

      unsetEnv "SKEMA_SERVER_PORT"
      unsetEnv "SKEMA_PORT"

      serverPort (server updatedCfg) @?= 9999

  , testCase "SKEMA_DATA_DIR (short form) overrides system data dir" $ do
      let cfg = defaultConfig
      unsetEnv "SKEMA_SYSTEM_DATA_DIR"
      setEnv "SKEMA_DATA_DIR" "/custom/data"

      updatedCfg <- applyEnvOverrides cfg

      unsetEnv "SKEMA_DATA_DIR"

      systemDataDir (system updatedCfg) @?= Just "/custom/data"
  ]

-- =============================================================================
-- Field to Env Var Name Conversion Tests
-- =============================================================================

fieldToEnvVarTests :: TestTree
fieldToEnvVarTests = testGroup "Field to Env Var Conversion"
  [ testCase "libraryPath -> SKEMA_LIBRARY_PATH" $
      fieldToEnvVar "libraryPath" @?= "SKEMA_LIBRARY_PATH"

  , testCase "serverPort -> SKEMA_SERVER_PORT" $
      fieldToEnvVar "serverPort" @?= "SKEMA_SERVER_PORT"

  , testCase "autoScanIntervalMins -> SKEMA_AUTO_SCAN_INTERVAL_MINS" $
      fieldToEnvVar "autoScanIntervalMins" @?= "SKEMA_AUTO_SCAN_INTERVAL_MINS"

  , testCase "mbUsername -> SKEMA_MB_USERNAME" $
      fieldToEnvVar "mbUsername" @?= "SKEMA_MB_USERNAME"

  -- fieldToEnvVars tests (with short form fallbacks)
  , testCase "serverPort -> [SKEMA_SERVER_PORT, SKEMA_PORT]" $
      fieldToEnvVars "serverPort" @?= ["SKEMA_SERVER_PORT", "SKEMA_PORT"]

  , testCase "serverHost -> [SKEMA_SERVER_HOST, SKEMA_HOST]" $
      fieldToEnvVars "serverHost" @?= ["SKEMA_SERVER_HOST", "SKEMA_HOST"]

  , testCase "serverUsername -> [SKEMA_SERVER_USERNAME, SKEMA_USERNAME]" $
      fieldToEnvVars "serverUsername" @?= ["SKEMA_SERVER_USERNAME", "SKEMA_USERNAME"]

  , testCase "systemDataDir -> [SKEMA_SYSTEM_DATA_DIR, SKEMA_DATA_DIR]" $
      fieldToEnvVars "systemDataDir" @?= ["SKEMA_SYSTEM_DATA_DIR", "SKEMA_DATA_DIR"]

  , testCase "systemCacheDir -> [SKEMA_SYSTEM_CACHE_DIR, SKEMA_CACHE_DIR]" $
      fieldToEnvVars "systemCacheDir" @?= ["SKEMA_SYSTEM_CACHE_DIR", "SKEMA_CACHE_DIR"]

  , testCase "libraryPath has no short form" $
      fieldToEnvVars "libraryPath" @?= ["SKEMA_LIBRARY_PATH"]

  , testCase "mbUsername has no short form" $
      fieldToEnvVars "mbUsername" @?= ["SKEMA_MB_USERNAME"]
  ]

-- =============================================================================
-- Config to API JSON Tests
-- =============================================================================

configToAPIJSONTests :: TestTree
configToAPIJSONTests = testGroup "Config to API JSON"
  [ testCase "adds auth_enabled=false when no credentials" $ do
      let cfg = defaultConfig

      jsonValue <- configToAPIJSON cfg

      -- Extract server.auth_enabled
      case jsonValue of
        Object obj -> case KM.lookup "server" obj of
          Just (Object srvObj) -> case KM.lookup "auth_enabled" srvObj of
            Just (Bool authEnabled) -> authEnabled @?= False
            other -> assertFailure $ "Expected Bool for auth_enabled, got: " <> show other
          other -> assertFailure $ "Expected Object for server, got: " <> show other
        other -> assertFailure $ "Expected Object, got: " <> show other

  , testCase "adds auth_enabled=true when credentials are set" $ do
      let cfg = defaultConfig
            { server = (server defaultConfig)
              { serverUsername = Just "admin"
              , serverPassword = Just "$2b$10$hashedpassword"
              }
            }

      jsonValue <- configToAPIJSON cfg

      case jsonValue of
        Object obj -> case KM.lookup "server" obj of
          Just (Object srvObj) -> case KM.lookup "auth_enabled" srvObj of
            Just (Bool authEnabled) -> authEnabled @?= True
            other -> assertFailure $ "Expected Bool for auth_enabled, got: " <> show other
          other -> assertFailure $ "Expected Object for server, got: " <> show other
        other -> assertFailure $ "Expected Object, got: " <> show other

  , testCase "removes password from response" $ do
      let cfg = defaultConfig
            { server = (server defaultConfig)
              { serverPassword = Just "secret"
              }
            }

      jsonValue <- configToAPIJSON cfg

      case jsonValue of
        Object obj -> case KM.lookup "server" obj of
          Just (Object srvObj) ->
            assertBool "password should not be in response" (not $ KM.member "password" srvObj)
          other -> assertFailure $ "Expected Object for server, got: " <> show other
        other -> assertFailure $ "Expected Object, got: " <> show other

  , testCase "removes jwt_secret from response" $ do
      let cfg = defaultConfig
            { server = (server defaultConfig)
              { serverJwtSecret = Just "supersecret"
              }
            }

      jsonValue <- configToAPIJSON cfg

      case jsonValue of
        Object obj -> case KM.lookup "server" obj of
          Just (Object srvObj) ->
            assertBool "jwt_secret should not be in response" (not $ KM.member "jwt_secret" srvObj)
          other -> assertFailure $ "Expected Object for server, got: " <> show other
        other -> assertFailure $ "Expected Object, got: " <> show other

  , testCase "preserves nested structure" $ do
      let cfg = defaultConfig

      jsonValue <- configToAPIJSON cfg

      case jsonValue of
        Object obj -> do
          assertBool "should have library section" (KM.member "library" obj)
          assertBool "should have server section" (KM.member "server" obj)
          assertBool "should have download section" (KM.member "download" obj)
          assertBool "should have musicbrainz section" (KM.member "musicbrainz" obj)
        other -> assertFailure $ "Expected Object, got: " <> show other
  ]

-- =============================================================================
-- Config Update Tests (JSON Merge)
-- =============================================================================

configUpdateTests :: TestTree
configUpdateTests = testGroup "Config JSON Update"
  [ testCase "updates single field" $ do
      let cfg = defaultConfig
      let update = object ["server" .= object ["port" .= (9000 :: Int)]]

      result <- applyConfigJSONUpdate cfg update Nothing

      case result of
        Right updatedCfg -> serverPort (server updatedCfg) @?= 9000
        Left err -> assertFailure $ "Update failed: " <> toString err

  , testCase "updates nested field without affecting others" $ do
      let cfg = defaultConfig
      let update = object ["server" .= object ["host" .= ("0.0.0.0" :: Text)]]

      result <- applyConfigJSONUpdate cfg update Nothing

      case result of
        Right updatedCfg -> do
          serverHost (server updatedCfg) @?= "0.0.0.0"
          -- Port should be unchanged
          serverPort (server updatedCfg) @?= 8182
        Left err -> assertFailure $ "Update failed: " <> toString err

  , testCase "updates multiple sections" $ do
      let cfg = defaultConfig
      let update = object
            [ "server" .= object ["port" .= (9000 :: Int)]
            , "library" .= object ["watch" .= False]
            ]

      result <- applyConfigJSONUpdate cfg update Nothing

      case result of
        Right updatedCfg -> do
          serverPort (server updatedCfg) @?= 9000
          libraryWatch (library updatedCfg) @?= False
        Left err -> assertFailure $ "Update failed: " <> toString err

  , testCase "uses hashed password when provided" $ do
      let cfg = defaultConfig
      let update = object ["server" .= object ["password" .= ("plaintext" :: Text)]]
      let hashedPwd = Just "$2b$10$hashedversion"

      result <- applyConfigJSONUpdate cfg update hashedPwd

      case result of
        Right updatedCfg ->
          serverPassword (server updatedCfg) @?= Just "$2b$10$hashedversion"
        Left err -> assertFailure $ "Update failed: " <> toString err

  , testCase "preserves fields not in update" $ do
      let cfg = defaultConfig
            { server = (server defaultConfig)
              { serverUsername = Just "admin"
              }
            }
      let update = object ["server" .= object ["port" .= (9000 :: Int)]]

      result <- applyConfigJSONUpdate cfg update Nothing

      case result of
        Right updatedCfg -> do
          serverPort (server updatedCfg) @?= 9000
          serverUsername (server updatedCfg) @?= Just "admin"
        Left err -> assertFailure $ "Update failed: " <> toString err

  , testCase "handles empty update" $ do
      let cfg = defaultConfig
      let update = object []

      result <- applyConfigJSONUpdate cfg update Nothing

      case result of
        Right updatedCfg ->
          serverPort (server updatedCfg) @?= 8182
        Left err -> assertFailure $ "Update failed: " <> toString err
  ]

-- =============================================================================
-- Schema Generation Tests
-- =============================================================================

schemaTests :: TestTree
schemaTests = testGroup "Schema Generation"
  [ testCase "schemaToJSON returns valid JSON with all sections" $ do
      let schemaJSON = schemaToJSON allSchemas

      case schemaJSON of
        Object obj -> do
          assertBool "should have library section" (KM.member "library" obj)
          assertBool "should have server section" (KM.member "server" obj)
          assertBool "should have download section" (KM.member "download" obj)
          assertBool "should have musicbrainz section" (KM.member "musicbrainz" obj)
        other -> assertFailure $ "Expected Object, got: " <> show other

  , testCase "each section has description and fields" $ do
      let schemaJSON = schemaToJSON allSchemas

      case schemaJSON of
        Object obj -> case KM.lookup "library" obj of
          Just (Object libObj) -> do
            assertBool "library should have description" (KM.member "description" libObj)
            assertBool "library should have fields" (KM.member "fields" libObj)
          other -> assertFailure $ "Expected Object for library, got: " <> show other
        other -> assertFailure $ "Expected Object, got: " <> show other

  , testCase "fields have name, description, and type" $ do
      let schemaJSON = schemaToJSON allSchemas

      case schemaJSON of
        Object obj -> case KM.lookup "library" obj of
          Just (Object libObj) -> case KM.lookup "fields" libObj of
            Just (Array fields) -> case viaNonEmpty head (toList fields) of
              Just (Object fieldObj) -> do
                assertBool "field should have name" (KM.member "name" fieldObj)
                assertBool "field should have description" (KM.member "description" fieldObj)
                assertBool "field should have type" (KM.member "type" fieldObj)
              Just other -> assertFailure $ "Expected Object for field, got: " <> show other
              Nothing -> assertFailure "Expected non-empty fields array"
            other -> assertFailure $ "Expected Array for fields, got: " <> show other
          other -> assertFailure $ "Expected Object for library, got: " <> show other
        other -> assertFailure $ "Expected Object, got: " <> show other
  ]
