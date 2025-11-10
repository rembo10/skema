{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Task management types for async operations.
module Skema.Tasks.Types
  ( TaskId
  , TaskType(..)
  , TaskStatus(..)
  , TaskProgress(..)
  , Task(..)
  , TaskResult(..)
  ) where

import GHC.Generics ()
import Data.Aeson (ToJSON(..), FromJSON(..), Options(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import Data.Time (UTCTime)

-- | Unique task identifier.
type TaskId = Text

-- | Type of task to execute.
data TaskType
  = LibraryScan
  | MusicBrainzIdentify
  | MetadataApply
  deriving (Show, Eq, Generic)

instance ToJSON TaskType
instance FromJSON TaskType

-- | Current status of a task.
data TaskStatus
  = TaskPending
  | TaskRunning
  | TaskCompleted
  | TaskFailed
  deriving (Show, Eq, Generic)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

-- | Progress information for a running task.
data TaskProgress = TaskProgress
  { progressMessage :: Text
  , progressPercent :: Maybe Int  -- 0-100
  , progressDetails :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TaskProgress where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance FromJSON TaskProgress where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Task record.
data Task = Task
  { taskId :: TaskId
  , taskType :: TaskType
  , taskStatus :: TaskStatus
  , taskProgress :: Maybe TaskProgress
  , taskResult :: Maybe TaskResult
  , taskCreatedAt :: UTCTime
  , taskStartedAt :: Maybe UTCTime
  , taskCompletedAt :: Maybe UTCTime
  , taskError :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance FromJSON Task where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

-- | Result data from completed tasks.
data TaskResult
  = LibraryScanResult
      { lsrFilesProcessed :: Int
      , lsrTotalGroups :: Int
      , lsrMatchedGroups :: Int
      , lsrFilesUpdated :: Int
      }
  | IdentifyResult
      { irTotalGroups :: Int
      , irMatchedGroups :: Int
      , irFilesUpdated :: Int
      }
  | MetadataApplyResult
      { marChangesApplied :: Int
      , marFilesUpdated :: Int
      }
  deriving (Show, Eq, Generic)

instance ToJSON TaskResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }

instance FromJSON TaskResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }
