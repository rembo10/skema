{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Task management API types.
module Skema.API.Types.Tasks
  ( TasksAPI
  , IdentifyRequest(..)
  , IdentifyResponse(..)
  , CreateTaskRequest(..)
  , CreateTaskResponse(..)
  ) where

import Skema.Tasks.Types (Task)
import Skema.API.Utils (createTaskRequestOptions, createTaskResponseOptions)
import GHC.Generics ()
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, defaultOptions, fieldLabelModifier, camelTo2)
import Servant

-- | Task management endpoints.
type TasksAPI = "tasks" :> Header "Authorization" Text :>
  ( ReqBody '[JSON] CreateTaskRequest :> PostCreated '[JSON] CreateTaskResponse
  :<|> Get '[JSON] [Task]
  :<|> Capture "taskId" Text :> Get '[JSON] Task
  )

-- | Request to identify files via MusicBrainz.
data IdentifyRequest = IdentifyRequest
  { identifyMinConfidence :: Maybe Double
    -- ^ Minimum confidence threshold (0.0-1.0), defaults to 0.5
  , identifyMaxCandidates :: Maybe Int
    -- ^ Maximum release candidates to consider, defaults to 5
  } deriving (Show, Eq, Generic)

instance ToJSON IdentifyRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance FromJSON IdentifyRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Response from MusicBrainz identification.
data IdentifyResponse = IdentifyResponse
  { identifySuccess :: Bool
  , identifyMessage :: Text
  , identifyTotalGroups :: Int
    -- ^ Total number of file groups (albums) processed
  , identifyMatchedGroups :: Int
    -- ^ Number of groups successfully matched
  , identifyFilesUpdated :: Int
    -- ^ Number of individual files updated with MusicBrainz IDs
  } deriving (Show, Eq, Generic)

instance ToJSON IdentifyResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance FromJSON IdentifyResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Request to create a new task.
data CreateTaskRequest = CreateTaskRequest
  { createTaskType :: Text
    -- ^ Task type: "library_scan" or "musicbrainz_identify"
  } deriving (Show, Eq, Generic)

instance ToJSON CreateTaskRequest where
  toJSON = genericToJSON createTaskRequestOptions

instance FromJSON CreateTaskRequest where
  parseJSON = genericParseJSON createTaskRequestOptions

-- | Response when creating a task.
data CreateTaskResponse = CreateTaskResponse
  { createTaskId :: Text
  , createTaskMessage :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateTaskResponse where
  toJSON = genericToJSON createTaskResponseOptions

instance FromJSON CreateTaskResponse where
  parseJSON = genericParseJSON createTaskResponseOptions
