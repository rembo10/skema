{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Task management system for async operations.
--
-- Tasks represent long-running asynchronous operations that can be:
-- - Created via resource-scoped endpoints (POST /library/tasks, etc.)
-- - Monitored via global endpoint (GET /tasks/{id})
-- - Tracked via SSE stream (GET /events)
--
-- The task manager maintains task state in memory and broadcasts
-- progress updates through the event bus.
module Skema.Core.TaskManager
  ( TaskManager
  , newTaskManager
  , createTask
  , getTask
  , getAllTasks
  , updateTaskProgress
  , completeTask
  , failTask
  , cancelTask
  , Task(..)
  ) where

import Skema.API.Types.Tasks (TaskResponse(..), TaskStatus(..), TaskResource(..))
import Skema.Events.Bus (EventBus, publishAndLog)
import Skema.Events.Types (Event)
import qualified Skema.Events.Types as Events
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TVar, readTVar, modifyTVar')
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, ToJSON(..), object, (.=))
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Katip (LogEnv)
import Data.Maybe (isNothing)
import Data.Function ((&))
import Data.Text (pack)

-- | Internal task representation.
data Task = Task
  { taskId :: Text
  , taskResource :: TaskResource
  , taskResourceId :: Maybe Int64
  , taskType :: Text
  , taskStatus :: TaskStatus
  , taskProgress :: Double
  , taskMessage :: Maybe Text
  , taskResult :: Maybe Value
  , taskError :: Maybe Text
  , taskCreatedAt :: UTCTime
  , taskStartedAt :: Maybe UTCTime
  , taskCompletedAt :: Maybe UTCTime
  } deriving (Show, Eq)

-- | Task manager with in-memory task storage and event bus integration.
data TaskManager = TaskManager
  { tmTasks :: TVar (Map Text Task)
    -- ^ In-memory task storage
  , tmEventBus :: EventBus
    -- ^ Event bus for broadcasting task updates
  , tmLogEnv :: LogEnv
    -- ^ Logging environment
  }

-- | Create a new task manager.
newTaskManager :: EventBus -> LogEnv -> IO TaskManager
newTaskManager bus le = do
  tasksVar <- STM.newTVarIO Map.empty
  pure $ TaskManager tasksVar bus le

-- | Create a new task and broadcast creation event.
createTask
  :: TaskManager
  -> TaskResource
  -> Maybe Int64  -- ^ Resource ID (e.g., clusterId, downloadId)
  -> Text         -- ^ Task type
  -> IO TaskResponse
createTask tm resource resourceId typ = do
  taskId' <- UUID.nextRandom
  now <- getCurrentTime

  let task = Task
        { taskId = UUID.toText taskId'
        , taskResource = resource
        , taskResourceId = resourceId
        , taskType = typ
        , taskStatus = TaskQueued
        , taskProgress = 0.0
        , taskMessage = Nothing
        , taskResult = Nothing
        , taskError = Nothing
        , taskCreatedAt = now
        , taskStartedAt = Nothing
        , taskCompletedAt = Nothing
        }

  -- Store task
  STM.atomically $ STM.modifyTVar' (tmTasks tm) (Map.insert (taskId task) task)

  -- Broadcast event
  publishAndLog (tmEventBus tm) (tmLogEnv tm) "task-manager" $
    Events.TaskCreated
      { Events.taskCreatedId = taskId task
      , Events.taskCreatedResource = show resource
      , Events.taskCreatedType = typ
      }

  pure $ taskToResponse task

-- | Get a task by ID.
getTask :: TaskManager -> Text -> IO (Maybe TaskResponse)
getTask tm tid = do
  tasks <- STM.readTVarIO (tmTasks tm)
  pure $ taskToResponse <$> Map.lookup tid tasks

-- | Get all tasks (optionally filtered).
getAllTasks :: TaskManager -> Maybe TaskResource -> Maybe TaskStatus -> IO [TaskResponse]
getAllTasks tm maybeResource maybeStatus = do
  tasks <- STM.readTVarIO (tmTasks tm)
  let filtered = Map.elems tasks
        & filter (\t -> maybe True (== taskResource t) maybeResource)
        & filter (\t -> maybe True (== taskStatus t) maybeStatus)
  pure $ map taskToResponse filtered

-- | Update task progress and broadcast progress event.
updateTaskProgress :: TaskManager -> Text -> Double -> Maybe Text -> IO ()
updateTaskProgress tm tid progress msg = do
  now <- getCurrentTime
  STM.atomically $ STM.modifyTVar' (tmTasks tm) $ Map.adjust
    (\t -> t
      { taskStatus = if taskStatus t == TaskQueued then TaskRunning else taskStatus t
      , taskProgress = progress
      , taskMessage = msg
      , taskStartedAt = if isNothing (taskStartedAt t) then Just now else taskStartedAt t
      })
    tid

  -- Broadcast progress event
  publishAndLog (tmEventBus tm) (tmLogEnv tm) "task-manager" $
    Events.TaskProgressUpdated
      { Events.taskProgressId = tid
      , Events.taskProgressValue = progress
      , Events.taskProgressMessage = msg
      }

-- | Mark task as completed and broadcast completion event.
completeTask :: TaskManager -> Text -> Maybe Value -> IO ()
completeTask tm tid result = do
  now <- getCurrentTime
  STM.atomically $ STM.modifyTVar' (tmTasks tm) $ Map.adjust
    (\t -> t
      { taskStatus = TaskCompleted
      , taskProgress = 1.0
      , taskResult = result
      , taskCompletedAt = Just now
      })
    tid

  -- Broadcast completion event
  publishAndLog (tmEventBus tm) (tmLogEnv tm) "task-manager" $
    Events.TaskCompleted
      { Events.taskCompletedId = tid
      , Events.taskCompletedResult = result
      }

-- | Mark task as failed and broadcast failure event.
failTask :: TaskManager -> Text -> Text -> IO ()
failTask tm tid errorMsg = do
  now <- getCurrentTime
  STM.atomically $ STM.modifyTVar' (tmTasks tm) $ Map.adjust
    (\t -> t
      { taskStatus = TaskFailed
      , taskError = Just errorMsg
      , taskCompletedAt = Just now
      })
    tid

  -- Broadcast failure event
  publishAndLog (tmEventBus tm) (tmLogEnv tm) "task-manager" $
    Events.TaskFailed'
      { Events.taskFailedId = tid
      , Events.taskFailedError = errorMsg
      }

-- | Cancel a running task.
cancelTask :: TaskManager -> Text -> IO Bool
cancelTask tm tid = do
  now <- getCurrentTime
  result <- STM.atomically $ do
    tasks <- STM.readTVar (tmTasks tm)
    case Map.lookup tid tasks of
      Nothing -> pure False
      Just task ->
        if taskStatus task `elem` [TaskQueued, TaskRunning]
        then do
          STM.modifyTVar' (tmTasks tm) $ Map.adjust
            (\t -> t
              { taskStatus = TaskCancelled
              , taskCompletedAt = Just now
              })
            tid
          pure True
        else pure False

  when result $
    publishAndLog (tmEventBus tm) (tmLogEnv tm) "task-manager" $
      Events.TaskCancelled'
        { Events.taskCancelledId = tid
        }

  pure result

-- | Convert internal Task to API TaskResponse.
taskToResponse :: Task -> TaskResponse
taskToResponse Task{..} = TaskResponse
  { taskResponseId = taskId
  , taskResponseResource = taskResource
  , taskResponseResourceId = taskResourceId
  , taskResponseType = taskType
  , taskResponseStatus = taskStatus
  , taskResponseProgress = taskProgress
  , taskResponseMessage = taskMessage
  , taskResponseResult = taskResult
  , taskResponseError = taskError
  , taskResponseCreatedAt = pack $ iso8601Show taskCreatedAt
  , taskResponseStartedAt = pack . iso8601Show <$> taskStartedAt
  , taskResponseCompletedAt = pack . iso8601Show <$> taskCompletedAt
  }
