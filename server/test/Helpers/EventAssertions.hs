{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Event assertion helpers for testing.
--
-- Provides utilities for waiting on events, asserting event sequences,
-- and checking event properties in tests.
module Helpers.EventAssertions
  ( -- * Event Waiting
    waitForEvent
  , waitForEventWithTimeout
  , waitForEvents
    -- * Event Predicates
  , isEventOfType
  , isLibraryScanRequested
  , isMetadataReadComplete
  , isClustersGenerated
  , isClusterIdentified
  , isCatalogArtistFollowed
  , isCatalogAlbumAdded
    -- * Event Assertions
  , assertEventEmitted
  , assertEventsInOrder
  , assertEventCount
    -- * Event Collection
  , collectEvents
  , collectEventsUntil
  ) where

import Skema.Events.Types
import Skema.Events.Bus (waitForEvent)
import Control.Concurrent.STM (TChan, readTChan, tryReadTChan)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import System.Timeout (timeout)

-- | Wait for a specific event matching a predicate.
--
-- Blocks until an event matching the predicate is received.
-- Returns the event or Nothing if timeout occurs.
waitForEventWithTimeout
  :: TChan EventEnvelope
  -> (Event -> Bool)
  -> Int  -- ^ Timeout in microseconds
  -> IO (Maybe Event)
waitForEventWithTimeout chan predicate timeoutMicros = do
  result <- timeout timeoutMicros $ go
  pure result
  where
    go = do
      envelope <- STM.atomically $ readTChan chan
      if predicate (envelopeEvent envelope)
        then pure (envelopeEvent envelope)
        else go

-- | Wait for multiple events matching predicates.
--
-- Waits for all predicates to match in any order.
-- Returns all matching events or Nothing if timeout.
waitForEvents
  :: TChan EventEnvelope
  -> [Event -> Bool]
  -> Int  -- ^ Timeout in microseconds
  -> IO (Maybe [Event])
waitForEvents chan predicates timeoutMicros = do
  result <- timeout timeoutMicros $ go predicates []
  pure result
  where
    go [] acc = pure (reverse acc)
    go remaining acc = do
      envelope <- STM.atomically $ readTChan chan
      let evt = envelopeEvent envelope
      -- Check if this event matches any remaining predicate
      case findMatching remaining evt of
        Just (_, otherPreds) ->
          go otherPreds (evt : acc)
        Nothing ->
          go remaining acc

    findMatching [] _ = Nothing
    findMatching (p:ps) evt
      | p evt = Just (p, ps)
      | otherwise = case findMatching ps evt of
          Just (matched, others) -> Just (matched, p : others)
          Nothing -> Nothing

-- | Check if an event is of a specific type.
isEventOfType :: Text -> Event -> Bool
isEventOfType typeName evt = eventType evt == typeName

-- | Check if event is LibraryScanRequested.
isLibraryScanRequested :: Event -> Bool
isLibraryScanRequested = \case
  LibraryScanRequested{} -> True
  _ -> False

-- | Check if event is MetadataReadComplete.
isMetadataReadComplete :: Event -> Bool
isMetadataReadComplete = \case
  MetadataReadComplete{} -> True
  _ -> False

-- | Check if event is ClustersGenerated.
isClustersGenerated :: Event -> Bool
isClustersGenerated = \case
  ClustersGenerated{} -> True
  _ -> False

-- | Check if event is ClusterIdentified.
isClusterIdentified :: Event -> Bool
isClusterIdentified = \case
  ClusterIdentified{} -> True
  _ -> False

-- | Check if event is CatalogArtistFollowed.
isCatalogArtistFollowed :: Event -> Bool
isCatalogArtistFollowed = \case
  CatalogArtistFollowed{} -> True
  _ -> False

-- | Check if event is CatalogAlbumAdded.
isCatalogAlbumAdded :: Event -> Bool
isCatalogAlbumAdded = \case
  CatalogAlbumAdded{} -> True
  _ -> False

-- | Assert that a specific event is emitted within timeout.
--
-- Throws an error if the event is not received.
assertEventEmitted
  :: TChan EventEnvelope
  -> (Event -> Bool)
  -> Int  -- ^ Timeout in microseconds
  -> IO ()
assertEventEmitted chan predicate timeoutMicros = do
  result <- waitForEventWithTimeout chan predicate timeoutMicros
  case result of
    Just _ -> pure ()
    Nothing -> error $ "Expected event was not emitted within " <> show (timeoutMicros `div` 1000) <> "ms"

-- | Assert that events are emitted in a specific order.
--
-- Allows other events to be interleaved, but the specified events
-- must appear in the given order.
assertEventsInOrder
  :: TChan EventEnvelope
  -> [Event -> Bool]
  -> Int  -- ^ Timeout in microseconds
  -> IO ()
assertEventsInOrder chan predicates timeoutMicros = do
  result <- timeout timeoutMicros $ go predicates
  case result of
    Just () -> pure ()
    Nothing -> error $ "Events did not appear in expected order within " <> show (timeoutMicros `div` 1000) <> "ms"
  where
    go [] = pure ()
    go (p:ps) = do
      envelope <- STM.atomically $ readTChan chan
      let evt = envelopeEvent envelope
      if p evt
        then go ps
        else go (p:ps)  -- Keep looking

-- | Assert that a specific number of events matching predicate are emitted.
assertEventCount
  :: TChan EventEnvelope
  -> (Event -> Bool)
  -> Int  -- ^ Expected count
  -> Int  -- ^ Timeout in microseconds
  -> IO ()
assertEventCount chan predicate expectedCount timeoutMicros = do
  events <- collectEventsUntil chan (const False) timeoutMicros
  let matchingCount = length $ filter predicate events
  when (matchingCount /= expectedCount) $
    error $ "Expected " <> show expectedCount <> " events, but got " <> show matchingCount

-- | Collect all events from a channel until a stop condition is met.
--
-- Useful for capturing all events during a test operation.
collectEventsUntil
  :: TChan EventEnvelope
  -> (Event -> Bool)  -- ^ Stop when this returns True
  -> Int              -- ^ Timeout in microseconds
  -> IO [Event]
collectEventsUntil chan stopPredicate timeoutMicros = do
  result <- timeout timeoutMicros $ go []
  case result of
    Just events -> pure (reverse events)
    Nothing -> pure []  -- Timeout, return what we got
  where
    go acc = do
      envelope <- STM.atomically $ readTChan chan
      let evt = envelopeEvent envelope
      if stopPredicate evt
        then pure (evt : acc)
        else go (evt : acc)

-- | Collect events for a specific duration.
--
-- Returns all events received during the time period.
collectEvents
  :: TChan EventEnvelope
  -> Int  -- ^ Duration in microseconds
  -> IO [Event]
collectEvents chan durationMicros = do
  result <- timeout durationMicros $ go []
  case result of
    Just events -> pure (reverse events)
    Nothing -> pure []
  where
    go acc = do
      maybeEnvelope <- STM.atomically $ tryReadTChan chan
      case maybeEnvelope of
        Just envelope -> go (envelopeEvent envelope : acc)
        Nothing -> do
          threadDelay 10000  -- Wait a bit and try again
          go acc
