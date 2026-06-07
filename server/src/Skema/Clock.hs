-- | An injectable source of the current time.
--
-- Production code uses 'systemClock', which reads the real wall clock.
-- Tests inject 'fixedClock' (a frozen instant) or 'mkClock' (a custom
-- 'IO' action, e.g. one backed by an 'Data.IORef.IORef' that advances on
-- demand) so that time-dependent behaviour becomes deterministic.
--
-- The clock is carried in 'Skema.Services.Types.ServiceContext' and the
-- per-service dependency records, so a service can be exercised end-to-end
-- under a controlled clock without touching global state.
module Skema.Clock
  ( Clock(..)
  , systemClock
  , fixedClock
  , mkClock
  ) where

import Data.Time (UTCTime, getCurrentTime)

-- | A source of the current time. Wraps an 'IO' action so the same record
-- field works for both the real clock and test doubles.
newtype Clock = Clock { getNow :: IO UTCTime }

-- | The real system clock, backed by 'getCurrentTime'.
systemClock :: Clock
systemClock = Clock getCurrentTime

-- | A clock frozen at a single instant. Every 'getNow' returns the same time.
fixedClock :: UTCTime -> Clock
fixedClock t = Clock (pure t)

-- | Build a clock from an arbitrary 'IO' action returning the current time.
-- Useful for tests that advance time on each read.
mkClock :: IO UTCTime -> Clock
mkClock = Clock
