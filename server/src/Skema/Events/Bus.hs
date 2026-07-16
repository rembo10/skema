{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Event bus for broadcasting domain events across the system.
module Skema.Events.Bus
  ( EventBus
  , newEventBus
  , publish
  , subscribe
  , publishAndLog
  , waitForEvent
  ) where

import Skema.Events.Types (Event(..), EventEnvelope(..), EventMetadata(..), eventType)
import Skema.Events.Utils (mkEventWithSequence)
import Control.Concurrent.STM (dupTChan, TChan, newBroadcastTChanIO, writeTChan, readTChan)
import qualified Control.Concurrent.STM as STM
import Katip (LogEnv, Severity(..), logStr, logTM)
import qualified Katip
import System.Timeout (timeout)

-- | Event bus using STM broadcast channels with sequence numbering.
--
-- Multiple subscribers can listen to the same events without interfering
-- with each other. Each subscriber gets their own "view" of the channel.
-- Events are assigned monotonically increasing sequence numbers for ordering.
data EventBus = EventBus
  { busChannel :: TChan EventEnvelope
  , busSequence :: TVar Int64  -- ^ Monotonically increasing sequence number
  }

-- | Create a new event bus.
newEventBus :: IO EventBus
newEventBus = do
  chan <- newBroadcastTChanIO
  seqVar <- STM.newTVarIO 0
  pure $ EventBus chan seqVar

-- | Publish an event to all subscribers.
--
-- Events are broadcast to all active subscribers. If there are no
-- subscribers, the event is still written but will be discarded.
publish :: EventBus -> EventEnvelope -> STM ()
publish bus envelope = writeTChan (busChannel bus) envelope

-- | Subscribe to events from the bus.
--
-- Returns a new TChan that will receive all events published after
-- subscription. Each subscriber gets an independent copy of events.
--
-- To filter specific event types, pattern match in your handler:
-- @
-- chan <- atomically $ subscribe bus
-- forever $ do
--   envelope <- atomically $ readTChan chan
--   case envelopeEvent envelope of
--     LibraryScanRequested{..} -> handleScan scanPath
--     _ -> pure ()  -- Ignore other events
-- @
subscribe :: EventBus -> STM (TChan EventEnvelope)
subscribe bus = dupTChan (busChannel bus)

-- | Publish an event and log it.
--
-- This is a convenience function that creates an event envelope,
-- publishes it to the bus, and logs it using Katip.
-- Assigns a monotonically increasing sequence number for ordering.
publishAndLog
  :: EventBus
  -> LogEnv
  -> Text  -- ^ Event source (e.g., "api", "task-runner")
  -> Event
  -> IO ()
publishAndLog bus le source evt = do
  -- Generate id/timestamp in IO, then allocate the sequence number and
  -- publish in a single transaction so channel order matches sequence order
  -- even with concurrent publishers
  templateEnvelope <- mkEventWithSequence source evt 0
  envelope <- STM.atomically $ do
    n <- readTVar (busSequence bus)
    modifyTVar' (busSequence bus) (+1)
    let sequenced = templateEnvelope
          { envelopeMetadata = (envelopeMetadata templateEnvelope) { eventSequence = n } }
    publish bus sequenced
    pure sequenced

  -- Log all events at debug level
  let initialContext = ()
  let initialNamespace = "events"
  Katip.runKatipContextT le initialContext initialNamespace $ do
    let EventMetadata{eventSequence = seqNum} = envelopeMetadata envelope
    $(Katip.logTM) DebugS $ logStr $
      "[events] #" <> show seqNum <> " " <> eventType evt <> " from " <> source

-- | Wait for a specific event matching a predicate.
--
-- Blocks until an event matching the predicate is received, or timeout occurs.
-- Returns Nothing on timeout (in microseconds).
waitForEvent :: TChan EventEnvelope -> (Event -> Bool) -> Int -> IO (Maybe Event)
waitForEvent chan predicate timeoutMicros = do
  result <- timeout timeoutMicros $ go
  pure result
  where
    go = do
      envelope <- STM.atomically $ readTChan chan
      if predicate (envelopeEvent envelope)
        then pure (envelopeEvent envelope)
        else go

