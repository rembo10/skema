{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Skema.Database.Types
import Skema.Database.Connection
import Skema.Database.Migrations
import Skema.API.Server
import Skema.Logging (initializeLogging, withKatipNamespace)
import Skema.Config.Discovery
import Skema.Config.EnvOverrides (lookupEnvField)
import Skema.Config.Types (Config(..), SystemConfig(..), LibraryConfig(..))
import Skema.Config.Directories
  ( SkemaDirectories(..)
  , DirectoryOverrides(..)
  , getSkemaDirectories
  )
import Skema.Events.Bus (newEventBus)
import Skema.Services.Registry (startAllServices)
import Skema.Services.AsyncRegistry (shutdownAllWithTimeout)
import Options.Applicative
import Katip
import Control.Monad.Catch (bracket)
import qualified System.OsPath as OP
import System.FilePath (isAbsolute, (</>))
import System.IO (hPutStrLn)
import System.Posix.Signals (installHandler, Handler(..), sigINT, sigTERM)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.Async (async, cancel, waitCatch)

-- | Command line options.
data Options = Options
  { optConfigFile :: Maybe FilePath
  , optDataDir :: Maybe FilePath
  , optCacheDir :: Maybe FilePath
  , optPort :: Maybe Int
  } deriving (Show)

-- | Command line parser.
optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption
      ( long "config"
     <> short 'c'
     <> metavar "FILE"
     <> help "Path to config file" ))
  <*> optional (strOption
      ( long "data-dir"
     <> metavar "DIR"
     <> help "Data directory (overrides config and XDG defaults)" ))
  <*> optional (strOption
      ( long "cache-dir"
     <> metavar "DIR"
     <> help "Cache directory (overrides config and XDG defaults)" ))
  <*> optional (option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Server port (overrides config and SKEMA_SERVER_PORT/SKEMA_PORT env var)" ))

-- | Main entry point.
main :: IO ()
main = do
  -- Parse command line options
  opts <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Skema music library manager"
   <> header "skema - music library management and acquisition" )

  -- Resolve port from CLI and environment (for config creation if needed)
  -- Priority: CLI > env var > default (config doesn't exist yet)
  -- Uses same env var logic as config system (SKEMA_SERVER_PORT or SKEMA_PORT)
  resolvedPort <- case optPort opts of
    Just p -> pure (Just p)
    Nothing -> do
      envPort <- lookupEnvField "serverPort"
      pure $ envPort >>= readMaybe

  -- Load configuration first (needed for directory overrides)
  -- Pass resolved port so new configs are created with the correct port
  configResult <- getOrCreateConfig (optConfigFile opts) resolvedPort
  case configResult of
    Left err -> do
      hPutStrLn stderr $ "Failed to load config: " <> toString err
      exitFailure
    Right (configPath, config) -> do
      let sysConfig = system config

      -- Build directory overrides from CLI and config
      -- Precedence: CLI > ENV > config > defaults (handled by getSkemaDirectories)
      let overrides = DirectoryOverrides
            { overrideDataDir = optDataDir opts
            , overrideCacheDir = optCacheDir opts
            , overrideLogFile = Nothing  -- Always use stdout
            , configDataDir = fmap toString (systemDataDir sysConfig)
            , configCacheDir = fmap toString (systemCacheDir sysConfig)
            }

      -- Resolve directories (CLI > ENV > config > platform defaults)
      dirs <- getSkemaDirectories overrides

      -- Initialize logging (stdout only)
      bracket initializeLogging closeScribes $ \le ->
        withKatipNamespace le "main" $ do
          $(logTM) InfoS $ logStr ("Starting Skema..." :: Text)
          $(logTM) InfoS $ logStr $ "Configuration loaded from: " <> toText configPath

          -- Log resolved directories
          $(logTM) InfoS $ logStr $ "Data directory: " <> toText (dataDir dirs)
          $(logTM) InfoS $ logStr $ "Cache directory: " <> toText (cacheDir dirs)

          -- Set up database from config
          -- If database path is relative, use the data directory
          let dbPathFromConfig = systemDatabasePath sysConfig
          let resolvedDbPath = if not (isAbsolute (toString dbPathFromConfig))
                then dataDir dirs </> toString dbPathFromConfig
                else toString dbPathFromConfig

          -- Log database location
          $(logTM) InfoS $ logStr $ "Database: " <> toText resolvedDbPath

          -- Database directory is already created by getSkemaDirectories

          let dbConfig = DatabaseConfig
                { dbPath = resolvedDbPath
                , dbPoolSize = 10
                }

          -- Create connection pool with automatic cleanup
          liftIO $ bracket
            (createConnectionPool le dbConfig)
            destroyConnectionPool
            $ \connPool -> do
              -- Run migrations
              runMigrations le connPool

              -- Create event bus
              bus <- newEventBus
              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("Event bus initialized" :: Text)

              -- Create shutdown signal MVar
              shutdownSignal <- MVar.newEmptyMVar

              -- Install signal handlers for graceful shutdown
              _ <- installHandler sigINT (Catch $ MVar.putMVar shutdownSignal "SIGINT") Nothing
              _ <- installHandler sigTERM (Catch $ MVar.putMVar shutdownSignal "SIGTERM") Nothing

              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("Signal handlers installed (SIGINT, SIGTERM)" :: Text)

              -- Start all services (now returns AsyncRegistry with all handles)
              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("Starting services..." :: Text)
              (serviceRegistry, asyncRegistry) <- startAllServices le bus connPool config (cacheDir dirs) configPath
              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("All services started" :: Text)

              -- Start API server
              let srvConfig = server config
              -- Extract library path as Text for the API
              let libConfig = library config
              libraryPathText <- case libraryPath libConfig of
                Nothing -> pure Nothing
                Just osPath -> do
                  pathStr <- OP.decodeUtf osPath
                  pure $ Just (toText pathStr)

              -- Start API server in async thread
              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("Starting API server..." :: Text)

              serverAsync <- async $ startServer le bus srvConfig serviceRegistry connPool libraryPathText (cacheDir dirs) configPath (optPort opts)

              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("Server running. Press Ctrl+C to stop." :: Text)

              -- Wait for shutdown signal or server exit
              signal <- MVar.takeMVar shutdownSignal

              -- Shutdown initiated
              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr $ ("Received " <> signal <> ", shutting down..." :: Text)

              -- Cancel the server async (sends ThreadKilled exception)
              cancel serverAsync

              -- Wait for server to finish (ignore result)
              _ <- waitCatch serverAsync

              -- Shutdown all services gracefully
              shutdownAllWithTimeout le asyncRegistry (30 * 1000000)

              runKatipContextT le () "main" $ do
                $(logTM) InfoS $ logStr ("Goodbye!" :: Text)
