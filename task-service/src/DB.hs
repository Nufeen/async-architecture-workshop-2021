{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DB where

import Types
import Kafka

import Servant --  (Handler, err404)
import qualified Control.Exception as CE
import Control.Monad.Except -- (throwError, liftIO, liftM)
import Hasql.Session (Session, statement, run, QueryError)
import Hasql.Statement (Statement(..))
import qualified Hasql.Connection as Connection
import qualified Hasql.Encoders as E
import Hasql.Decoders
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras.Contrazip (contrazip2)
import GHC.Int -- (Int64)

noPrepare :: Bool
noPrepare = False

connect :: Session a -> IO (Either QueryError a)
connect session  = do
  Right connection <- Connection.acquire settings
  result <- run (session) connection
  return result
  where
    settings = Connection.settings "localhost" 5432 "postgres" "" "workshopdb"

runSession :: Session b -> Handler b
runSession session = do
  res <- liftIO $ connect session
  case res of
    Left _ -> throwError err404
    Right r -> return r

taskDecoder :: Row Task
taskDecoder = Task
  <$> column (nonNullable int8)
  <*> column (nonNullable text)
  <*> column (nonNullable text)
  <*> column (nonNullable bool)

getTasks :: Handler [Task]
getTasks = runSession $ statement () s
  where
    s = Statement query encoder decoder noPrepare
    query = "SELECT * FROM tasks"
    encoder = E.noParams
    decoder = rowList taskDecoder

getOpenedTasksIds :: IO (Either QueryError [Int64])
getOpenedTasksIds = connect $ statement () s
  where
    s = Statement query encoder decoder noPrepare
    query = "select taskid from tasks where opened = 't';"
    encoder = E.noParams
    decoder = rowList $ column $ nonNullable int8

addTask :: TaskData -> Handler Int64
addTask (TaskData name description) = do
  res <- liftIO $ connect $ statement (name, description) s
  case res of
    Left _ -> throwError err404
    -- в жизни так писать не надо, это надо разносить
    -- на отдельное апи и отдельный слой контроллеров
    -- также стоит обогащать событие метаданными
    -- и контролировать договоренности о схемах событий
    Right r -> liftIO $ Kafka.runProducer "taskCreated" (show r) >> pure r
  where
    s = Statement query encoder decoder noPrepare
    query = "insert into tasks (task_name, description, opened) \
            \values ($1, $2, 'TRUE') returning taskid"
    encoder = contrazip2
      (E.param $ E.nonNullable E.text)
      (E.param $ E.nonNullable E.text)
    decoder = singleRow $ column $ nonNullable int8

closeTask :: Int64 -> Handler Status
closeTask (taskid) = do
  res <- liftIO $ connect $ statement (taskid) s
  case res of
    Left _ -> return $ Err "Failed to delete"
    Right 0 -> return $ Err "Task doesn't exist"
    Right r -> do
      liftIO $ Kafka.runProducer "taskClosed" (show taskid)
      return $ Confirm "Task closed"
  where
    s = Statement query encoder decoder noPrepare
    query = "UPDATE tasks SET opened = 'f' WHERE taskid = 1"
    encoder = (E.param $ E.nonNullable E.int8)
    decoder = rowsAffected

reassignTasks :: Handler Status
reassignTasks = do
  ids <- liftIO getOpenedTasksIds
  case ids of
    Left _ -> return $ Err ""
    Right tasks -> do
      _ <- (mapM_ reassign tasks)
      return $ Confirm "Reassign Success"

reassign :: MonadIO m => Int64 -> m Status
reassign task = do
  assignee <- liftIO getRandomUserId
  case assignee of
    Left _ -> return $ Err "DB connect error or no user exist"
    Right a -> do
      res <- liftIO $ updateAssignee task a
      case res of
        Left _ -> return $ Err "Failed to reassign"
        Right 0 -> return $ Err "Task doesn't exist"
        Right r ->  do
          liftIO $ Kafka.runProducer "taskReassigned" (show task)
          return $ Confirm "Success"

updateAssignee :: Int64 -> Int64 -> IO (Either QueryError Int64)
updateAssignee taskid assignee =
  connect $ statement (taskid, assignee) s
  where
    s = Statement query encoder decoder noPrepare
    query = "UPDATE tasks SET assignee = $2 WHERE taskid = $1"
    encoder = contrazip2
      (E.param $ E.nonNullable E.int8)
      (E.param $ E.nonNullable E.int8)
    decoder = rowsAffected

getRandomUserId :: IO (Either QueryError Int64)
getRandomUserId = connect $ statement () s
  where
    s = Statement query encoder decoder noPrepare
    -- Не делайте так в проде, это медленно!
    -- Slow but short request
    -- For educational code only, dont do this in production!
    query = "select userid from users ORDER BY random() LIMIT 1;"
    encoder = E.noParams
    decoder = singleRow $ column $ nonNullable int8
