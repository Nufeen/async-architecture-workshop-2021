{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Handlers where

import qualified DB as DB
import Kafka
import Types

import Servant  (Handler, err404, err500, errBody)
import Control.Monad.Except (throwError, liftIO,)
import GHC.Int (Int64)

getTasks :: Handler [Task]
getTasks = do
  res <- liftIO DB.getTasks
  case res of
    Left _ -> throwError err500 {errBody = "Database error"}
    Right r -> return r

addTask :: TaskData -> Handler Int64
addTask (TaskData name description) = do
  res <- liftIO $ DB.addTask (TaskData name description)
  case res of
    Left _ -> throwError err500 {errBody = "Task add failed"}
    -- В жизни стоит обогащать событие метаданными
    -- и контролировать договоренности о схемах событий
    -- Также в проде стоит задуматься о public_id не в интах
    Right r -> liftIO $ Kafka.runProducer "taskCreated" (show r) >> pure r

closeTask :: Int64 -> Handler Status
closeTask (taskid) = do
  res <- liftIO $ DB.closeTask (taskid)
  case res of
    Left _ -> return $ Err "Failed to delete"
    Right 0 -> return $ Err "Task doesn't exist"
    Right r -> do
      liftIO $ Kafka.runProducer "taskClosed" (show taskid)
      return $ Confirm "Task closed"

reassignAll :: Handler Status
reassignAll = do
  ids <- liftIO DB.getOpenedTasksIds
  case ids of
    Left _ -> return $ Err ""
    Right tasks -> do
      _ <- (mapM_ reassign tasks)
      return $ Confirm "Reassign Success"

reassign :: Int64 -> Handler Status
reassign task = do
  assignee <- liftIO DB.getRandomUserId
  case assignee of
    Left _ -> return $ Err "DB connect error or no user exist"
    Right a -> do
      res <- liftIO $ DB.updateAssignee task a
      case res of
        Left _ -> return $ Err "Failed to reassign"
        Right 0 -> return $ Err "Task doesn't exist"
        Right r -> do
          -- Тут же запускали бы сайд-эффект с нотификацией
          liftIO $ Kafka.runProducer "taskReassigned" (show task)
          return $ Confirm "Success"
