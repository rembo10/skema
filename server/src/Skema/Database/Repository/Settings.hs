{-# LANGUAGE OverloadedStrings #-}

-- | Settings repository operations.
module Skema.Database.Repository.Settings
  ( getDefaultQualityProfileId
  , setDefaultQualityProfileId
  ) where

import Skema.Database.Connection
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

-- | Get the default quality profile ID from settings.
getDefaultQualityProfileId :: SQLite.Connection -> IO (Maybe Int64)
getDefaultQualityProfileId conn = do
  results <- queryRows conn
    "SELECT default_quality_profile_id FROM settings WHERE id = 1"
    () :: IO [Only (Maybe Int64)]
  case viaNonEmpty head results of
    Just (Only profileId) -> pure profileId
    Nothing -> pure Nothing

-- | Set the default quality profile ID in settings.
setDefaultQualityProfileId :: SQLite.Connection -> Maybe Int64 -> IO ()
setDefaultQualityProfileId conn maybeProfileId = do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE settings SET default_quality_profile_id = ?, updated_at = ? WHERE id = 1"
    (maybeProfileId, now)
