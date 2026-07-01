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
  :: ConnectionPool               -- ^ Connection pool
  -> (SQLite.Connection -> IO a)  -- ^ Action to perform with DB connection
  -> Handler a
withDB connPool action = liftIO $ withConnection connPool action

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
