{-# LANGUAGE OverloadedStrings #-}

-- | Global tasks API handlers.
module Skema.API.Handlers.Tasks
  ( tasksServer
  ) where

import Skema.API.Types.Tasks (TasksAPI)
import Skema.API.Types.Tasks (TaskResponse)
import Skema.Services.TaskManager (TaskManager)
import qualified Skema.Services.TaskManager as TM
import Servant

-- | Global tasks API handlers.
tasksServer :: TaskManager -> Server TasksAPI
tasksServer tm =
  listTasksHandler
  :<|> getTaskHandler
  :<|> cancelTaskHandler
  where
    listTasksHandler :: Handler [TaskResponse]
    listTasksHandler =
      liftIO $ TM.getAllTasks tm Nothing Nothing

    getTaskHandler :: Text -> Handler TaskResponse
    getTaskHandler taskId = do
      maybeTask <- liftIO $ TM.getTask tm taskId
      case maybeTask of
        Nothing -> throwError err404 { errBody = "Task not found" }
        Just task -> pure task

    cancelTaskHandler :: Text -> Handler NoContent
    cancelTaskHandler taskId = do
      success <- liftIO $ TM.cancelTask tm taskId
      if success
        then pure NoContent
        else throwError err404 { errBody = "Task not found or already completed" }
