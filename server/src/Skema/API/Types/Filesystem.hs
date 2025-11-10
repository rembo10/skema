{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Filesystem browsing API types.
module Skema.API.Types.Filesystem
  ( FilesystemAPI
  , FilesystemEntry(..)
  , FilesystemBrowseResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Filesystem browsing API endpoints.
type FilesystemAPI = "filesystem" :> Header "Authorization" Text :>
  ( "browse" :> QueryParam "path" Text :> Get '[JSON] FilesystemBrowseResponse
  )

-- | Filesystem entry (file or directory).
data FilesystemEntry = FilesystemEntry
  { filesystemEntryName :: Text
    -- ^ Name of the file or directory
  , filesystemEntryPath :: Text
    -- ^ Full path to the file or directory
  , filesystemEntryIsDirectory :: Bool
    -- ^ Whether this is a directory
  , filesystemEntrySize :: Maybe Integer
    -- ^ File size in bytes (Nothing for directories)
  , filesystemEntryModifiedAt :: Maybe Text
    -- ^ Last modified timestamp
  , filesystemEntryReadable :: Bool
    -- ^ Whether the entry is readable
  , filesystemEntryWritable :: Bool
    -- ^ Whether the entry is writable
  } deriving (Show, Eq, Generic)

instance ToJSON FilesystemEntry where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON FilesystemEntry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

-- | Response from filesystem browse endpoint.
data FilesystemBrowseResponse = FilesystemBrowseResponse
  { filesystemBrowseResponsePath :: Text
    -- ^ Current directory path
  , filesystemBrowseResponseParent :: Maybe Text
    -- ^ Parent directory path (Nothing if at root)
  , filesystemBrowseResponseEntries :: [FilesystemEntry]
    -- ^ List of entries in the directory
  , filesystemBrowseResponseError :: Maybe Text
    -- ^ Error message if browsing failed
  } deriving (Show, Eq, Generic)

instance ToJSON FilesystemBrowseResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 24 }

instance FromJSON FilesystemBrowseResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 24 }
