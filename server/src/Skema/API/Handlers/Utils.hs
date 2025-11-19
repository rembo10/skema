{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for API handlers to reduce repetitive patterns.
module Skema.API.Handlers.Utils
  ( withAuthDB
  , withAuth
  ) where

import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection (ConnectionPool, withConnection)
import qualified Skema.Config.Types as Cfg
import Servant (Handler)
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
