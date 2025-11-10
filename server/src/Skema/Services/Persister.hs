{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Persister service - finalizes scan results.
module Skema.Services.Persister
  ( startPersisterService
  ) where

import Skema.Services.Dependencies (PersisterDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Katip

-- | Start the persister service.
--
-- This service listens for IDENTIFICATION_COMPLETE events and emits final RESULTS_PERSISTED event.
startPersisterService :: PersisterDeps -> IO (Async ())
startPersisterService deps = do
  chan <- STM.atomically $ subscribe (persistEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      IdentificationComplete{..} -> handleIdentificationComplete deps matchesFound
      _ -> pure ()  -- Ignore other events

-- | Handle an identification complete event.
handleIdentificationComplete :: PersisterDeps -> Int -> IO ()
handleIdentificationComplete PersisterDeps{..} matches = do
  let le = persistLogEnv
  let bus = persistEventBus
  let initialContext = ()
  let initialNamespace = "services.persister"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr $ ("Finalizing scan results: " <> show matches <> " albums matched" :: Text)

    -- Emit ResultsPersisted event
    liftIO $ publishAndLog bus le "persister" $ ResultsPersisted
      { filesUpdated = matches  -- This is a simplification - we should count actual files
      }
