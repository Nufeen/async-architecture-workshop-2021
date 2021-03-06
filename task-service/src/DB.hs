{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DB where

import Types
import Kafka

import Servant  (Handler, err404)
import qualified Control.Exception as CE
import Control.Monad.Except (throwError, liftIO,)
import Hasql.Session (Session, statement, run, QueryError)
import Hasql.Statement (Statement(..))
import qualified Hasql.Connection as Connection
import qualified Hasql.Encoders as E
import Hasql.Decoders
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras.Contrazip (contrazip2)
import GHC.Int (Int64)

noPrepare :: Bool
noPrepare = False

connect :: Session a -> IO (Either QueryError a)
connect session  = do
  Right connection <- Connection.acquire settings
  result <- run (session) connection
  return result
  where
    settings = Connection.settings "localhost" 5432 "postgres" "" "workshopdb"

taskDecoder :: Row Task
taskDecoder = Task
  <$> column (nonNullable int8)
  <*> column (nonNullable text)
  <*> column (nonNullable text)
  <*> column (nonNullable bool)

getTasks :: IO (Either QueryError [Task])
getTasks = connect $ statement () s
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

addTask :: TaskData -> IO (Either QueryError Int64)
addTask (TaskData name description) =
  connect $ statement (name, description) s
  where
    s = Statement query encoder decoder noPrepare
    query = "insert into tasks (task_name, description, opened) \
            \values ($1, $2, 'TRUE') returning taskid"
    encoder = contrazip2
      (E.param $ E.nonNullable E.text)
      (E.param $ E.nonNullable E.text)
    decoder = singleRow $ column $ nonNullable int8

closeTask :: Int64 -> IO (Either QueryError Int64)
closeTask taskid =
   connect $ statement (taskid) s
  where
    s = Statement query encoder decoder noPrepare
    query = "UPDATE tasks SET opened = 'f' WHERE taskid = 1"
    encoder = (E.param $ E.nonNullable E.int8)
    decoder = rowsAffected

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
    -- ???? ?????????????? ?????? ?? ??????????, ?????? ????????????????!
    -- Slow but short request
    -- For educational code only, dont do this in production!
    query = "select userid from users ORDER BY random() LIMIT 1;"
    encoder = E.noParams
    decoder = singleRow $ column $ nonNullable int8
