{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DB where

import Types

import Servant (Handler, err404)
import Control.Monad.Except
import Hasql.Session (Session, statement, run, QueryError)
import Hasql.Statement (Statement(..))
import qualified Hasql.Connection as Connection
import qualified Hasql.Encoders as E
import Hasql.Decoders
import Data.Functor.Contravariant ((>$<))
import Contravariant.Extras.Contrazip (contrazip2)
import GHC.Int (Int64)
import Data.Text (Text, unpack)
import Data.Password.Bcrypt

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

userDecoder :: Row User
userDecoder = User
  <$> column (nonNullable int8)
  <*> column (nonNullable text)
  <*> column (nullable bool)
  <*> column (nullable bool)
  <*> column (nonNullable text)

getUsers :: Handler [User]
getUsers = runSession $ statement () s
  where
    s = Statement query encoder decoder noPrepare
    query = "SELECT * FROM users"
    encoder = E.noParams
    decoder = rowList userDecoder

addUser :: AuthData -> Handler Status
addUser (AuthData name pass) = do
  bcryptedpass <- hashPassword $ mkPassword pass
  res <- liftIO $ connect $ statement (name, unPasswordHash bcryptedpass) s
  case res of
    Left _ -> return $ Err "User creation fail"
    Right _ -> return $ Confirm "User created successfully"
  where
    s = Statement query encoder decoder noPrepare
    query = "INSERT INTO users (name, pass) VALUES ($1, $2) RETURNING userid;"
    encoder = contrazip2
      (E.param $ E.nonNullable E.text)
      (E.param $ E.nonNullable E.text)
    decoder = singleRow $ column $ nonNullable int8

authCheck :: AuthData -> IO Status
authCheck (AuthData name password) = do
  res <- liftIO $ connect $ statement (name) s
  print res
  case res of
    Left _ -> return $ Err "Fail"
    Right [] -> return $ Err "User doesnt exist"
    Right [u] -> validate u password
  where
    s = Statement query encoder decoder noPrepare
    query = "select * from users where name=$1"
    encoder = (E.param $ E.nonNullable E.text)
    decoder = rowList userDecoder

-- https://hackage.haskell.org/package/password-2.0.1.1/docs/Data-Password-Bcrypt.html#g:4
validate :: Monad m => User -> Text -> m Status
validate u password = do
  let h = PasswordHash {unPasswordHash = pass u}
  let p = mkPassword password
  let r =checkPassword p h
  case r of
    PasswordCheckSuccess -> return $ Confirm "OK"
    _ -> return $ Err "Wrong password"
