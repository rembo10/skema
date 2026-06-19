{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for testing Servant API handlers without a running WAI server.
--
-- A handler is a @'Handler' a@ (i.e. @ExceptT ServerError IO a@). 'runOk'
-- and 'runStatus' run one against the real handler stack and assert on the
-- success value or the HTTP error code respectively. 'dummyJWTSecret'
-- supplies the 'JWTSecret' a handler signature requires; its value is never
-- inspected because auth is disabled under 'defaultConfig'.
--
-- Each spec still wires its own handlers (the API-specific Servant ':<|>'
-- destructure) and applies a 'Nothing' auth header.
module Helpers.Handler
  ( dummyJWTSecret
  , runOk
  , runStatus
  ) where

import Test.Tasty.HUnit (assertFailure)
import qualified Data.Text as T
import Servant (Handler, ServerError(..))
import Servant.Server (runHandler)

import Skema.Auth.JWT (JWTSecret, getJWTSecret)
import qualified Skema.Config.Types as Cfg

-- | Build a real 'JWTSecret' for tests. Its value is never inspected
-- because auth is disabled under 'Cfg.defaultConfig', but handler
-- signatures require one.
dummyJWTSecret :: IO JWTSecret
dummyJWTSecret = do
  let cfg = (Cfg.server Cfg.defaultConfig) { Cfg.serverJwtSecret = Just (T.replicate 64 "0") }
  maybe (assertFailure "could not build test JWT secret") pure =<< getJWTSecret cfg

-- | Run a handler expecting success, returning the response value.
runOk :: Handler a -> IO a
runOk h = either (assertFailure . ("unexpected error response: " <>) . show) pure =<< runHandler h

-- | Run a handler expecting a failure, returning the HTTP status code.
runStatus :: Handler a -> IO Int
runStatus h = do
  result <- runHandler h
  case result of
    Left err -> pure (errHTTPCode err)
    Right _ -> assertFailure "expected an error response, got success"
