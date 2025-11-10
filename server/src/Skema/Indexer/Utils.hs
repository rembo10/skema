{-# LANGUAGE OverloadedStrings #-}

module Skema.Indexer.Utils
  ( downloadTypeFromMimeType
  ) where

import Data.Text ()
import qualified Data.Text as T
import Skema.Indexer.Types (DownloadType(..))

-- | Convert MIME type to download type
downloadTypeFromMimeType :: Text -> Maybe DownloadType
downloadTypeFromMimeType mime
  | "nzb" `T.isInfixOf` T.toLower mime = Just NZB
  | "torrent" `T.isInfixOf` T.toLower mime = Just Torrent
  | otherwise = Nothing
