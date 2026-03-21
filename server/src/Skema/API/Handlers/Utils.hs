{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for API handlers to reduce repetitive patterns.
module Skema.API.Handlers.Utils
  ( withAuthDB
  , withAuth
  , throwJsonError
  , throw400
  , throw401
  , throw404
  , throw500
  , readConfig
  , parsePagination
  ) where

import Skema.API.Types.Common (mkErrorResponse)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection (ConnectionPool, withConnection)
import qualified Skema.Config.Types as Cfg
import qualified Control.Concurrent.STM as STM
import Servant
import Data.Aeson (encode)
import qualified Database.SQLite.Simple as SQLite

-- | Execute a handler action with authentication and database connection.
-- This combines the common pattern of:
--   _ <- requireAuth configVar jwtSecret authHeader
--   liftIO $ withConnection connPool $ \conn -> ...
--
-- Usage:
--   myHandler authHeader params = withAuthDB configVar jwtSecret connPool authHeader $ \conn ->
--     -- handler logic with database connection
withAuthDB
  :: TVar Cfg.Config          -- ^ Config variable
  -> JWTSecret                -- ^ JWT secret
  -> ConnectionPool           -- ^ Connection pool
  -> Maybe Text               -- ^ Auth header
  -> (SQLite.Connection -> IO a)  -- ^ Action to perform with DB connection
  -> Handler a
withAuthDB configVar jwtSecret connPool authHeader action = do
  _ <- requireAuth configVar jwtSecret authHeader
  liftIO $ withConnection connPool action

-- | Execute a handler action with authentication only (no database).
-- Provides access to the authenticated username (if auth is enabled).
--
-- Usage:
--   myHandler authHeader params = withAuth configVar jwtSecret authHeader $ \username ->
--     -- handler logic with username (Maybe Text)
withAuth
  :: TVar Cfg.Config          -- ^ Config variable
  -> JWTSecret                -- ^ JWT secret
  -> Maybe Text               -- ^ Auth header
  -> (Maybe Text -> Handler a)  -- ^ Action to perform with username
  -> Handler a
withAuth configVar jwtSecret authHeader action = do
  username <- requireAuth configVar jwtSecret authHeader
  action username

-- | Helper to throw a JSON error with a specific status code.
throwJsonError :: ServerError -> Text -> Handler a
throwJsonError statusErr msg = throwError $ statusErr
  { errBody = encode $ mkErrorResponse msg
  , errHeaders = [("Content-Type", "application/json")]
  }

-- | Throw a 400 Bad Request error.
throw400 :: Text -> Handler a
throw400 = throwJsonError err400

-- | Throw a 401 Unauthorized error.
throw401 :: Text -> Handler a
throw401 = throwJsonError err401

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Read the current configuration from a TVar.
readConfig :: TVar Cfg.Config -> IO Cfg.Config
readConfig = STM.atomically . STM.readTVar

-- | Parse pagination parameters with defaults (offset=0, limit=50).
parsePagination :: Maybe Int -> Maybe Int -> (Int, Int)
parsePagination maybeOffset maybeLimit = (fromMaybe 0 maybeOffset, fromMaybe 50 maybeLimit)
