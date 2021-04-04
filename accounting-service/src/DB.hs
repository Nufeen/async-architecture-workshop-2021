{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DB where

import Types

import Servant (Handler, err404)
import Control.Monad.Except (throwError, liftIO)
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

-- TODO Endpoints

-- GET:

-- - getTransactions
-- - getUserInvoice
-- - getDailyManagementIncome
-- - getGoldenTask

-- POST:

-- - writeOff
-- - charge
-- - clearBalanceSheet
