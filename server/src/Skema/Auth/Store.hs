{-# LANGUAGE OverloadedStrings #-}

-- | Authentication store for managing API keys.
module Skema.Auth.Store
  ( -- * Storage
    AuthStore
  , newAuthStore
  , storeApiKey
  , validateApiKey
  , invalidateApiKey
  ) where

import Skema.Auth.Types (ApiKey(..), AuthStore(..), ApiKeyInfo(..))
import Data.Time (getCurrentTime, addUTCTime)
import Control.Concurrent.STM ()
import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict as Map
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

-- | Create a new empty auth store.
newAuthStore :: IO AuthStore
newAuthStore = AuthStore <$> STM.newTVarIO Map.empty

-- | Generate a new API key.
generateApiKey :: IO ApiKey
generateApiKey = ApiKey . UUID.toText <$> UUID.nextRandom

-- | Store a new API key with optional expiration.
--
-- If expiration is Nothing, the key never expires.
-- Returns the generated API key.
storeApiKey :: AuthStore -> Maybe Int -> IO ApiKey
storeApiKey (AuthStore store) maybeExpirationSecs = do
  apiKey <- generateApiKey
  now <- getCurrentTime

  let expiresAt = case maybeExpirationSecs of
        Nothing -> Nothing
        Just secs -> Just $ addUTCTime (fromIntegral secs) now

  let keyInfo = ApiKeyInfo
        { apiKeyValue = apiKey
        , apiKeyCreatedAt = now
        , apiKeyExpiresAt = expiresAt
        }

  STM.atomically $ modifyTVar' store $ Map.insert apiKey keyInfo
  pure apiKey

-- | Validate an API key.
--
-- Checks if the key exists and hasn't expired.
validateApiKey :: AuthStore -> ApiKey -> IO Bool
validateApiKey (AuthStore store) key = do
  now <- getCurrentTime
  keyMap <- STM.atomically $ readTVar store

  case Map.lookup key keyMap of
    Nothing -> pure False
    Just keyInfo ->
      case apiKeyExpiresAt keyInfo of
        Nothing -> pure True
        Just expiresAt -> pure (now < expiresAt)

-- | Invalidate (delete) an API key.
invalidateApiKey :: AuthStore -> ApiKey -> IO ()
invalidateApiKey (AuthStore store) key =
  STM.atomically $ modifyTVar' store $ Map.delete key
