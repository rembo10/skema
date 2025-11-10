{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Utility functions for API types.
module Skema.API.Utils
  ( createTaskRequestOptions
  , createTaskResponseOptions
  ) where

import Data.Aeson (Options(..), defaultOptions)

-- | JSON options for CreateTaskRequest - strips "createTask" prefix
createTaskRequestOptions :: Options
createTaskRequestOptions = defaultOptions
  { fieldLabelModifier = \field -> case field of
      "createTaskType" -> "type"
      _ -> field
  }

-- | JSON options for CreateTaskResponse - strips "createTask" prefix
createTaskResponseOptions :: Options
createTaskResponseOptions = defaultOptions
  { fieldLabelModifier = \field -> case field of
      "createTaskId" -> "id"
      "createTaskMessage" -> "message"
      _ -> field
  }
