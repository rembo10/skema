{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The standard event-service loop.
--
-- Every event-driven service is a subscription plus a match function; this
-- module owns everything else: the subscribe/read loop, exception logging,
-- and the execution mode. The returned handle is the loop thread itself, so
-- registering it with the AsyncRegistry gives shutdown real control.
module Skema.Services.EventLoop
  ( JobMode (..)
  , runEventService
  ) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (readTChan)
import qualified Control.Concurrent.STM as STM
import Control.Exception (try)
import Katip

import Skema.Events.Bus (EventBus, subscribe)
import Skema.Events.Types (Event, EventEnvelope (..), eventType)

-- | How matched jobs are executed.
data JobMode
  = Sequential
    -- ^ One job at a time in the loop thread; later events queue on the
    -- subscription channel until the current job finishes. The right choice
    -- whenever jobs mutate shared state (regrouping, imports, file writes).
  | Concurrent
    -- ^ Each job runs in its own thread. Only for independent, IO-bound work
    -- whose overlap is safe — typically because the real resource is already
    -- serialized elsewhere (HTTP rate limits, per-key locks).

-- | Run an event-driven service: subscribe to the bus and execute the job
-- returned by the match function for each event. Exceptions from jobs are
-- logged under the service's namespace and never kill the loop.
runEventService
  :: LogEnv
  -> EventBus
  -> Text                       -- ^ Service name, used as the log namespace
  -> JobMode
  -> (Event -> Maybe (IO ()))   -- ^ Match an event to the job handling it
  -> IO (Async ())
runEventService le bus name mode match = do
  chan <- STM.atomically $ subscribe bus
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    let evt = envelopeEvent envelope
    forM_ (match evt) $ \job ->
      case mode of
        Sequential -> runJob evt job
        Concurrent -> void $ async $ runJob evt job
  where
    runJob :: Event -> IO () -> IO ()
    runJob evt job = do
      result <- try job
      case result of
        Left (e :: SomeException) ->
          runKatipContextT le () (Namespace [name]) $
            $(logTM) ErrorS $ logStr $
              ("Exception handling " <> eventType evt <> ": " <> show e :: Text)
        Right () -> pure ()
