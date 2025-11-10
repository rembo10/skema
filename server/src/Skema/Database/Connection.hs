{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Database connection management (SQLite only).
module Skema.Database.Connection
  ( -- * Connection Pool
    ConnectionPool
  , createConnectionPool
  , destroyConnectionPool
  , withConnection
    -- * Query Execution
  , executeQuery
  , executeQuery_
  , queryRows
  , queryRows_
  ) where

import Skema.Database.Types
import qualified Database.SQLite.Simple as SQLite
import Data.Pool (Pool, newPool, destroyAllResources, withResource, defaultPoolConfig, setNumStripes)
import Katip

-- | Constants
connectionIdleTimeoutSeconds :: Double
connectionIdleTimeoutSeconds = 60

busyTimeoutMilliseconds :: Int
busyTimeoutMilliseconds = 5000

-- | Connection pool for SQLite.
type ConnectionPool = Pool SQLite.Connection

-- | Create a connection pool for SQLite.
createConnectionPool :: LogEnv -> DatabaseConfig -> IO ConnectionPool
createConnectionPool le config = do
  let initialContext = ()
  let initialNamespace = "database"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr ("Connecting to SQLite database: " <> toText (dbPath config) :: Text)

  newPool $ setNumStripes (Just 1) $ defaultPoolConfig
    (do
      conn <- SQLite.open (dbPath config)
      -- Enable WAL mode for better concurrency
      SQLite.execute_ conn "PRAGMA journal_mode=WAL"
      -- Set busy timeout
      SQLite.execute_ conn $ "PRAGMA busy_timeout=" <> show busyTimeoutMilliseconds
      -- Enable foreign keys
      SQLite.execute_ conn "PRAGMA foreign_keys=ON"
      pure conn
    )
    SQLite.close
    connectionIdleTimeoutSeconds
    (dbPoolSize config)

-- | Destroy a connection pool.
destroyConnectionPool :: ConnectionPool -> IO ()
destroyConnectionPool = destroyAllResources

-- | Run an action with a connection from the pool.
withConnection :: ConnectionPool -> (SQLite.Connection -> IO a) -> IO a
withConnection = withResource

-- | Execute a statement with parameters (no result expected).
executeQuery
  :: SQLite.ToRow q
  => SQLite.Connection
  -> Text
  -> q
  -> IO ()
executeQuery conn sql params =
  SQLite.execute conn (fromString $ toString sql) params

-- | Execute a statement without parameters.
executeQuery_
  :: SQLite.Connection
  -> Text
  -> IO ()
executeQuery_ conn sql =
  SQLite.execute_ conn (fromString $ toString sql)

-- | Query with parameters.
queryRows
  :: (SQLite.ToRow q, SQLite.FromRow r)
  => SQLite.Connection
  -> Text
  -> q
  -> IO [r]
queryRows conn sql params =
  SQLite.query conn (fromString $ toString sql) params

-- | Query without parameters.
queryRows_
  :: SQLite.FromRow r
  => SQLite.Connection
  -> Text
  -> IO [r]
queryRows_ conn sql =
  SQLite.query_ conn (fromString $ toString sql)
