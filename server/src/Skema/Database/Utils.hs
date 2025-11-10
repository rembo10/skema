{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for database types.
--
-- NOTE: This module now re-exports from Database.Types to eliminate
-- the circular dependency that previously required a .hs-boot file.
module Skema.Database.Utils
  ( sourceTypeToText
  , textToSourceType
  , albumStatusToText
  , textToAlbumStatus
  , downloadStatusToText
  , textToDownloadStatus
  ) where

import Skema.Database.Types
  ( sourceTypeToText
  , textToSourceType
  , albumStatusToText
  , textToAlbumStatus
  , downloadStatusToText
  , textToDownloadStatus
  )
