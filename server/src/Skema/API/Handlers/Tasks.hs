{-# LANGUAGE OverloadedStrings #-}

-- | Global tasks API handlers.
module Skema.API.Handlers.Tasks
  ( tasksServer
  ) where

import Skema.API.Types.Tasks (TasksAPI)
import Skema.API.Types.Tasks (TaskResponse)
import qualified Skema.Services.TaskManager as TM
import Servant
import Skema.API.App (Env(..), AppM)

-- | Global tasks API handlers.
tasksServer :: Env -> ServerT TasksAPI AppM
tasksServer Env{envTaskManager = tm} =
  listTasksHandler
  :<|> getTaskHandler
  :<|> cancelTaskHandler
  where
    listTasksHandler :: AppM [TaskResponse]
    listTasksHandler =
      liftIO $ TM.getAllTasks tm Nothing Nothing

    getTaskHandler :: Text -> AppM TaskResponse
    getTaskHandler taskId = do
      maybeTask <- liftIO $ TM.getTask tm taskId
      case maybeTask of
        Nothing -> throwError err404 { errBody = "Task not found" }
        Just task -> pure task

    cancelTaskHandler :: Text -> AppM NoContent
    cancelTaskHandler taskId = do
      success <- liftIO $ TM.cancelTask tm taskId
      if success
        then pure NoContent
        else throwError err404 { errBody = "Task not found or already completed" }
