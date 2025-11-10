{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Async registry for tracking background threads and enabling graceful shutdown.
module Skema.Services.AsyncRegistry
  ( AsyncRegistry
  , AsyncHandle(..)
  , newAsyncRegistry
  , registerAsync
  , shutdownAll
  , shutdownAllWithTimeout
  ) where

import Control.Concurrent.Async (Async, cancel, waitCatch, race, AsyncCancelled)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM ()
import Control.Monad ()
import Control.Exception ()
import Katip

-- | A handle representing a registered async operation.
data AsyncHandle = AsyncHandle
  { ahName :: Text  -- ^ Human-readable name for debugging
  , ahAsync :: Async ()  -- ^ The async handle
  } deriving (Eq)

-- | Registry for tracking all async operations in the application.
newtype AsyncRegistry = AsyncRegistry
  { arHandles :: TVar [AsyncHandle]
  }

-- | Create a new empty async registry.
newAsyncRegistry :: IO AsyncRegistry
newAsyncRegistry = AsyncRegistry <$> STM.newTVarIO []

-- | Register an async operation with the registry.
-- The async operation must already be started.
registerAsync :: AsyncRegistry -> Text -> Async () -> IO ()
registerAsync registry name asyncHandle = do
  STM.atomically $ modifyTVar' (arHandles registry) (AsyncHandle name asyncHandle :)

-- | Shutdown all registered async operations gracefully.
-- Cancels all operations and waits for them to finish.
shutdownAll :: LogEnv -> AsyncRegistry -> IO ()
shutdownAll le registry = do
  handles <- STM.atomically $ readTVar (arHandles registry)

  runKatipContextT le () "async-registry" $ do
    -- Cancel all operations
    liftIO $ forM_ handles $ \AsyncHandle{..} -> do
      runKatipContextT le () "async-registry" $ do
        $(logTM) DebugS $ logStr $ ("Cancelling: " <> ahName :: Text)
      cancel ahAsync

    -- Wait for all to finish
    liftIO $ forM_ handles $ \AsyncHandle{..} -> do
      result <- waitCatch ahAsync
      runKatipContextT le () "async-registry" $ do
        case result of
          Left ex ->
            -- AsyncCancelled is expected during shutdown, don't log as warning
            case fromException ex :: Maybe AsyncCancelled of
              Just _ -> $(logTM) DebugS $ logStr $ ("Async '" <> ahName <> "' cancelled" :: Text)
              Nothing -> $(logTM) WarningS $ logStr $ ("Async '" <> ahName <> "' terminated with exception: " <> show ex :: Text)
          Right () -> $(logTM) DebugS $ logStr $ ("Async '" <> ahName <> "' terminated cleanly" :: Text)

    $(logTM) InfoS $ logStr ("All services stopped" :: Text)

-- | Shutdown all registered async operations with a timeout.
-- If operations don't finish within the timeout, remaining threads are left running.
-- Timeout is in microseconds.
shutdownAllWithTimeout :: LogEnv -> AsyncRegistry -> Int -> IO ()
shutdownAllWithTimeout le registry timeoutMicros = do
  -- Get handle count for logging
  handles <- STM.atomically $ readTVar (arHandles registry)
  runKatipContextT le () "async-registry" $ do
    $(logTM) InfoS $ logStr $ ("Stopping " <> show (length handles) <> " background services..." :: Text)

  -- Race between shutdown and timeout
  result <- race (threadDelay timeoutMicros) (shutdownAll le registry)

  case result of
    Left () -> runKatipContextT le () "async-registry" $ do
      $(logTM) WarningS $ logStr $ ("Shutdown timeout exceeded (>" <> show (timeoutMicros `div` 1000000) <> "s), some services may still be running" :: Text)
    Right () -> pure ()  -- shutdownAll already logged "All async operations stopped"
