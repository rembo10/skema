{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for API handlers to reduce repetitive patterns.
module Skema.API.Handlers.Utils
  ( withDB
  , throwJsonError
  , throw400
  , throw401
  , throw404
  , throw500
  , readConfig
  , parsePagination
  ) where

import Skema.API.Types.Common (mkErrorResponse)
import Skema.Database.Connection (ConnectionPool, withConnection)
import qualified Skema.Config.Types as Cfg
import qualified Control.Concurrent.STM as STM
import Servant
import Control.Monad.Error.Class (MonadError)
import Data.Aeson (encode)
import qualified Database.SQLite.Simple as SQLite

-- | Run an action with a database connection inside a 'Handler'.
--
-- Authentication is enforced centrally by the auth middleware, so handlers no
-- longer gate on it themselves.
--
-- Usage:
--   myHandler params = withDB connPool $ \conn -> ...
withDB
  :: MonadIO m
  => ConnectionPool               -- ^ Connection pool
  -> (SQLite.Connection -> IO a)  -- ^ Action to perform with DB connection
  -> m a
withDB connPool action = liftIO $ withConnection connPool action

-- | Helper to throw a JSON error with a specific status code.
throwJsonError :: MonadError ServerError m => ServerError -> Text -> m a
throwJsonError statusErr msg = throwError $ statusErr
  { errBody = encode $ mkErrorResponse msg
  , errHeaders = [("Content-Type", "application/json")]
  }

-- | Throw a 400 Bad Request error.
throw400 :: MonadError ServerError m => Text -> m a
throw400 = throwJsonError err400

-- | Throw a 401 Unauthorized error.
throw401 :: MonadError ServerError m => Text -> m a
throw401 = throwJsonError err401

-- | Throw a 404 Not Found error.
throw404 :: MonadError ServerError m => Text -> m a
throw404 = throwJsonError err404

-- | Throw a 500 Internal Server Error.
throw500 :: MonadError ServerError m => Text -> m a
throw500 = throwJsonError err500

-- | Read the current configuration from a TVar.
readConfig :: MonadIO m => TVar Cfg.Config -> m Cfg.Config
readConfig = liftIO . STM.atomically . STM.readTVar

-- | Parse pagination parameters with defaults (offset=0, limit=50).
parsePagination :: Maybe Int -> Maybe Int -> (Int, Int)
parsePagination maybeOffset maybeLimit = (fromMaybe 0 maybeOffset, fromMaybe 50 maybeLimit)
