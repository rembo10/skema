{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for working with events.
module Skema.Events.Utils
  ( mkEventWithSequence
  ) where

import Data.Time (getCurrentTime)
import qualified Data.UUID.V4 as UUID

import Skema.Events.Types
  ( Event
  , EventMetadata(..)
  , EventEnvelope(..)
  )

-- | Create an event with generated metadata and a specific sequence number.
-- The sequence number should be obtained from the EventBus to ensure monotonicity.
mkEventWithSequence :: Text -> Event -> Int64 -> IO EventEnvelope
mkEventWithSequence source evt eventSequence = do
  eventId <- UUID.nextRandom
  eventTimestamp <- getCurrentTime
  pure $ EventEnvelope
    { envelopeMetadata = EventMetadata{eventSource = source, ..}
    , envelopeEvent = evt
    }
