{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Metadata diffs API types.
module Skema.API.Types.Diffs
  ( DiffsAPI
  , MetadataDiffResponse(..)
  , GroupedDiffResponse(..)
  , GroupedDiffsResponse(..)
  , DiffsPagination(..)
  , ApplyGroupedDiffRequest(..)
  , ApplyToFileRequest(..)
  , ApplyChangesRequest(..)
  , MetadataChangeResponse(..)
  , MetadataChangesResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Metadata diffs API endpoints.
type DiffsAPI = "diffs" :> Header "Authorization" Text :>
  ( Get '[JSON] [MetadataDiffResponse]
  :<|> "grouped"
    :> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] GroupedDiffsResponse
  :<|> "apply-grouped" :> ReqBody '[JSON] ApplyGroupedDiffRequest :> Post '[JSON] ()
  :<|> "apply-to-file" :> ReqBody '[JSON] ApplyToFileRequest :> Post '[JSON] ()
  )
  :<|> "metadata-changes" :> Header "Authorization" Text :>
  ( ReqBody '[JSON] ApplyChangesRequest :> PostCreated '[JSON] [MetadataChangeResponse]
  :<|> QueryParam "offset" Int
    :> QueryParam "limit" Int
    :> Get '[JSON] MetadataChangesResponse
  :<|> Capture "changeId" Int64 :> DeleteNoContent
  )

-- | Metadata diff response.
data MetadataDiffResponse = MetadataDiffResponse
  { diffId :: Int64
  , diffTrackId :: Int64
  , diffFilePath :: Text
  , diffFieldName :: Text
  , diffFileValue :: Maybe Text
  , diffMBValue :: Maybe Text
  , diffCreatedAt :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON MetadataDiffResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance FromJSON MetadataDiffResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

-- | Grouped diff response.
data GroupedDiffResponse = GroupedDiffResponse
  { groupedFieldName :: Text
  , groupedFileValue :: Maybe Text
  , groupedMBValue :: Maybe Text
  , groupedCount :: Int
  , groupedTrackIds :: [Int64]
  , groupedDiffs :: [MetadataDiffResponse]
  } deriving (Show, Eq, Generic)

instance ToJSON GroupedDiffResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7 }

instance FromJSON GroupedDiffResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | Pagination info for diffs.
data DiffsPagination = DiffsPagination
  { diffsPaginationTotal :: Int
  , diffsPaginationOffset :: Int
  , diffsPaginationLimit :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DiffsPagination where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON DiffsPagination where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

-- | Paginated grouped diffs response.
data GroupedDiffsResponse = GroupedDiffsResponse
  { groupedDiffsResponsePagination :: DiffsPagination
  , groupedDiffsResponseDiffs :: [GroupedDiffResponse]
  } deriving (Show, Eq, Generic)

instance ToJSON GroupedDiffsResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

instance FromJSON GroupedDiffsResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

-- | Request to apply grouped diff.
data ApplyGroupedDiffRequest = ApplyGroupedDiffRequest
  { applyFieldName :: Text
  , applyFileValue :: Maybe Text
  , applyMBValue :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ApplyGroupedDiffRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromJSON ApplyGroupedDiffRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Request to apply diff to a single track.
data ApplyToFileRequest = ApplyToFileRequest
  { applyToTrackId :: Int64
  , applyToFieldName :: Text
  , applyToValue :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ApplyToFileRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7 }

instance FromJSON ApplyToFileRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | Request to apply changes (creating metadata change records).
data ApplyChangesRequest = ApplyChangesRequest
  { applyChangesDiffIds :: [Int64]
    -- ^ List of diff IDs to apply
  } deriving (Show, Eq, Generic)

instance ToJSON ApplyChangesRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON ApplyChangesRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

-- | Metadata change response (for history and undo).
data MetadataChangeResponse = MetadataChangeResponse
  { changeResponseId :: Int64
  , changeResponseTrackId :: Int64
  , changeResponseFilePath :: Text
  , changeResponseFieldName :: Text
  , changeResponseOldValue :: Maybe Text
  , changeResponseNewValue :: Maybe Text
  , changeResponseAppliedAt :: Text
  , changeResponseReverted :: Bool
    -- ^ Whether this change has been reverted
  } deriving (Show, Eq, Generic)

instance ToJSON MetadataChangeResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 14 }

instance FromJSON MetadataChangeResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 14 }

-- | Paginated metadata changes response.
data MetadataChangesResponse = MetadataChangesResponse
  { metadataChangesResponsePagination :: DiffsPagination
  , metadataChangesResponseChanges :: [MetadataChangeResponse]
  } deriving (Show, Eq, Generic)

instance ToJSON MetadataChangesResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

instance FromJSON MetadataChangesResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }
