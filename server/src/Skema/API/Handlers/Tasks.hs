{-# LANGUAGE OverloadedStrings #-}

-- | Global tasks API handlers.
module Skema.API.Handlers.Tasks
  ( tasksServer
  ) where

import Skema.API.Types.Tasks (TasksAPI)
import Skema.Core.TaskManager (TaskManager)
import qualified Skema.Core.TaskManager as TM
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Config.Types (Config)
import Servant

-- | Global tasks API handlers.
tasksServer :: JWTSecret -> TaskManager -> TVar Config -> Server TasksAPI
tasksServer jwtSecret tm configVar = \maybeAuthHeader ->
  listTasksHandler maybeAuthHeader
  :<|> getTaskHandler maybeAuthHeader
  :<|> cancelTaskHandler maybeAuthHeader
  where
    listTasksHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ TM.getAllTasks tm Nothing Nothing

    getTaskHandler authHeader taskId = do
      _ <- requireAuth configVar jwtSecret authHeader
      maybeTask <- liftIO $ TM.getTask tm taskId
      case maybeTask of
        Nothing -> throwError err404 { errBody = "Task not found" }
        Just task -> pure task

    cancelTaskHandler authHeader taskId = do
      _ <- requireAuth configVar jwtSecret authHeader
      success <- liftIO $ TM.cancelTask tm taskId
      if success
        then pure NoContent
        else throwError err404 { errBody = "Task not found or already completed" }
