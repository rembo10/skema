-- | The API application monad and its environment.
--
-- Every handler runs in 'AppM', a 'ReaderT' over 'Env'. The environment is
-- built once in Skema.API.Server and hoisted into the Servant server, so
-- handler modules take a single 'Env' instead of threading each dependency
-- positionally.
module Skema.API.App
  ( Env (..)
  , AppM
  , toServiceContext
  ) where

import Katip (LogEnv)
import Servant (Handler)

import Skema.Services.UpdateChecker (LatestRelease)
import Skema.Auth.JWT (JWTSecret)
import Skema.Clock (Clock)
import Skema.Config.Types (Config)
import Skema.Database.Connection (ConnectionPool)
import Skema.Events.Bus (EventBus)
import Skema.HTTP.Client (HttpClient)
import Skema.MusicBrainz.Client (MBClientEnv)
import Skema.Services.TaskManager (TaskManager)
import Skema.Services.Types (ServiceContext (..))

-- | Everything a request handler can depend on.
data Env = Env
  { envLogEnv :: LogEnv
  , envEventBus :: EventBus
  , envJwtSecret :: JWTSecret
  , envTaskManager :: TaskManager
  , envDbPool :: ConnectionPool
  , envConfigVar :: TVar Config
    -- ^ Live configuration; read this rather than a startup snapshot
  , envHttpClient :: HttpClient
  , envMBClient :: MBClientEnv
  , envClock :: Clock
  , envProgressMap :: TVar (Map Int64 (Double, Text))
    -- ^ In-memory download progress (download_id -> (progress, display status))
  , envLibraryPath :: Maybe Text
  , envCacheDir :: FilePath
  , envConfigPath :: FilePath
  , envLatestRelease :: TVar (Maybe LatestRelease)
    -- ^ Cached update-checker result
  }

-- | The handler monad: 'Env' over Servant's 'Handler'.
type AppM = ReaderT Env Handler

-- | View the handler environment as a service context, so handlers can call
-- service-layer functions directly (e.g. for API-triggered tasks).
toServiceContext :: Env -> ServiceContext
toServiceContext env = ServiceContext
  { scEventBus = envEventBus env
  , scDbPool = envDbPool env
  , scConfigVar = envConfigVar env
  , scLogEnv = envLogEnv env
  , scHttpClient = envHttpClient env
  , scMBClientEnv = envMBClient env
  , scCacheDir = envCacheDir env
  , scDownloadProgressMap = envProgressMap env
  , scClock = envClock env
  }
