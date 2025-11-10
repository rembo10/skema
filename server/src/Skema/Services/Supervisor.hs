{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Service supervision and error handling.
--
-- This module provides resilient service execution with:
-- - Automatic exception catching and logging
-- - Exponential backoff for retries
-- - Graceful error recovery
module Skema.Services.Supervisor
  ( superviseService
  , superviseEventHandler
  , withRecovery
  , RetryConfig(..)
  , defaultRetryConfig
  ) where

import qualified Control.Exception as E
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled)
import Control.Monad ()
import Katip

-- | Retry configuration for service supervision.
data RetryConfig = RetryConfig
  { retryInitialDelay :: Int
    -- ^ Initial delay in microseconds (default: 1 second)
  , retryMaxDelay :: Int
    -- ^ Maximum delay in microseconds (default: 60 seconds)
  , retryBackoffMultiplier :: Double
    -- ^ Backoff multiplier (default: 2.0 for exponential backoff)
  } deriving (Show, Eq)

-- | Default retry configuration.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryInitialDelay = 1000000      -- 1 second
  , retryMaxDelay = 60000000         -- 60 seconds
  , retryBackoffMultiplier = 2.0
  }

-- | Supervise a service with automatic restart on failure.
--
-- If the service action throws an exception, it will be caught, logged,
-- and the service will be restarted after a delay with exponential backoff.
superviseService
  :: LogEnv
  -> Text                -- ^ Service name for logging
  -> RetryConfig
  -> IO ()               -- ^ Service action
  -> IO ()
superviseService le serviceName config action = do
  let initialContext = ()
  let initialNamespace = "services.supervisor"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr $ "Starting supervised service: " <> serviceName

  loop (retryInitialDelay config)
  where
    loop currentDelay = do
      result <- E.try action :: IO (Either E.SomeException ())

      case result of
        Right () -> do
          -- Service exited normally (shouldn't happen with forever loops)
          runKatipContextT le () "services.supervisor" $ do
            $(logTM) WarningS $ logStr $
              "Service exited normally: " <> serviceName <> " (restarting)"
          threadDelay currentDelay
          loop (retryInitialDelay config)

        Left err -> do
          -- Don't restart on AsyncCancelled - this means we're shutting down
          case E.fromException err :: Maybe AsyncCancelled of
            Just _ -> do
              runKatipContextT le () "services.supervisor" $ do
                $(logTM) InfoS $ logStr $
                  "Service cancelled: " <> serviceName <> " (shutdown)"
              E.throwIO err  -- Re-throw to propagate cancellation
            Nothing -> do
              runKatipContextT le () "services.supervisor" $ do
                $(logTM) ErrorS $ logStr $
                  "Service crashed: " <> serviceName <> " - " <> show err <>
                  " (restarting in " <> show (currentDelay `div` 1000000) <> "s)"

              threadDelay currentDelay

              -- Calculate next delay with exponential backoff
              let nextDelay = min
                    (retryMaxDelay config)
                    (round $ fromIntegral currentDelay * retryBackoffMultiplier config)

              loop nextDelay

-- | Supervise an event handler with exception recovery.
--
-- Wraps an event processing function to catch and log exceptions
-- without crashing the service. Returns True if handler succeeded,
-- False if it failed (for optional retry logic).
superviseEventHandler
  :: LogEnv
  -> Text                -- ^ Handler name for logging
  -> IO ()               -- ^ Event handler action
  -> IO Bool             -- ^ Returns True on success, False on failure
superviseEventHandler le handlerName action = do
  result <- E.try action :: IO (Either E.SomeException ())

  case result of
    Right () -> pure True
    Left err -> do
      let initialContext = ()
      let initialNamespace = "services.supervisor"
      runKatipContextT le initialContext initialNamespace $ do
        $(logTM) ErrorS $ logStr $
          "Event handler failed: " <> handlerName <> " - " <> show err <>
          " (continuing)"
      pure False

-- | Execute an action with automatic exception recovery.
--
-- If the action throws an exception, it will be caught and logged.
-- Returns Nothing on failure, Just result on success.
withRecovery
  :: LogEnv
  -> Text                -- ^ Operation name for logging
  -> IO a                -- ^ Action to execute
  -> IO (Maybe a)        -- ^ Returns Just result or Nothing on failure
withRecovery le opName action = do
  result <- E.try action

  case result of
    Right value -> pure $ Just value
    Left (err :: E.SomeException) -> do
      let initialContext = ()
      let initialNamespace = "services.recovery"
      runKatipContextT le initialContext initialNamespace $ do
        $(logTM) ErrorS $ logStr $
          "Operation failed: " <> opName <> " - " <> show err
      pure Nothing
