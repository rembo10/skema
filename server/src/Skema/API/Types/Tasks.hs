{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Tasks API types.
--
-- Tasks represent asynchronous operations across the application.
-- Each resource (library, clusters, catalog, downloads) can create tasks,
-- and all tasks can be monitored through the global /tasks endpoint.
module Skema.API.Types.Tasks
  ( TasksAPI
  , TaskResponse(..)
  , TaskStatus(..)
  , TaskResource(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Global tasks API endpoints.
type TasksAPI = "tasks" :> Header "Authorization" Text :>
  ( Get '[JSON] [TaskResponse]
  :<|> Capture "taskId" Text :> Get '[JSON] TaskResponse
  :<|> Capture "taskId" Text :> DeleteNoContent
  )

-- | Task status enumeration.
data TaskStatus
  = TaskQueued
  | TaskRunning
  | TaskCompleted
  | TaskFailed
  | TaskCancelled
  deriving (Show, Eq, Generic)

instance ToJSON TaskStatus where
  toJSON TaskQueued = "queued"
  toJSON TaskRunning = "running"
  toJSON TaskCompleted = "completed"
  toJSON TaskFailed = "failed"
  toJSON TaskCancelled = "cancelled"

instance FromJSON TaskStatus where
  parseJSON "queued" = pure TaskQueued
  parseJSON "running" = pure TaskRunning
  parseJSON "completed" = pure TaskCompleted
  parseJSON "failed" = pure TaskFailed
  parseJSON "cancelled" = pure TaskCancelled
  parseJSON _ = fail "Invalid task status"

-- | Resource that created the task.
data TaskResource
  = LibraryResource
  | ClustersResource
  | CatalogResource
  | DownloadsResource
  deriving (Show, Eq, Generic)

instance ToJSON TaskResource where
  toJSON LibraryResource = "library"
  toJSON ClustersResource = "clusters"
  toJSON CatalogResource = "catalog"
  toJSON DownloadsResource = "downloads"

instance FromJSON TaskResource where
  parseJSON "library" = pure LibraryResource
  parseJSON "clusters" = pure ClustersResource
  parseJSON "catalog" = pure CatalogResource
  parseJSON "downloads" = pure DownloadsResource
  parseJSON _ = fail "Invalid task resource"

-- | Task response (for both creation and status queries).
data TaskResponse = TaskResponse
  { taskResponseId :: Text
    -- ^ Unique task ID
  , taskResponseResource :: TaskResource
    -- ^ Resource that created this task
  , taskResponseResourceId :: Maybe Int64
    -- ^ Specific resource ID if applicable (e.g., clusterId, downloadId)
  , taskResponseType :: Text
    -- ^ Task type (e.g., "scan", "identify", "refresh", "reidentify")
  , taskResponseStatus :: TaskStatus
    -- ^ Current task status
  , taskResponseProgress :: Double
    -- ^ Progress from 0.0 to 1.0
  , taskResponseMessage :: Maybe Text
    -- ^ Human-readable status message
  , taskResponseResult :: Maybe Value
    -- ^ Task result data (populated on completion)
  , taskResponseError :: Maybe Text
    -- ^ Error message (populated on failure)
  , taskResponseCreatedAt :: Text
    -- ^ When task was created
  , taskResponseStartedAt :: Maybe Text
    -- ^ When task started running
  , taskResponseCompletedAt :: Maybe Text
    -- ^ When task completed/failed
  } deriving (Show, Eq, Generic)

instance ToJSON TaskResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON TaskResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }
