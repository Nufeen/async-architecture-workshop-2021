{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Types
import qualified DB as DB

import Servant
import Servant.API.Generic (genericApi, ToServantApi, (:-))
import Servant.Server.Generic (genericServer, AsServer)

import Data.Swagger (Swagger, title, info)
import Servant.Swagger (toSwagger)
import Control.Lens ((.~), (&))
import GHC.Generics (Generic)
import GHC.Int (Int64)

import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.HTTP.Client hiding (Proxy)
import Network.Wai.Handler.Warp

import Servant.Client
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)
import Data.Map as M
import Data.ByteString (ByteString)

import Data.Text (Text, unpack )
import Data.Text.Encoding

import           Network.HTTP.Simple hiding (Proxy)
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Exception (bracket)
import Kafka.Consumer


data AuthenticatedUser = AUser { auID :: Int
                               , auOrgID :: Int
                               } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser


data Routes route = Routes
  { _users :: route :- "users" :> Get '[JSON] [User]
  , _add :: route :- "add" :> ReqBody '[JSON] AuthData :> Put '[JSON] Status
  } deriving (Generic)

routes :: Routes AsServer
routes = Routes
  { _users = DB.getUsers
  , _add = DB.addUser
  }

test :: Handler Text
test = httpBS "http://localhost:8090/tasks" >>= return . decodeUtf8 . getResponseBody

data Gates route = Gates {
  _tasks :: route :- "tasks" :> Get '[PlainText] Text
  } deriving (Generic)

gates :: Gates AsServer
gates = Gates {
  _tasks = test
  }

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type API = ToServantApi Routes :<|> ToServantApi Gates :<|> SwaggerAPI

app :: Application
app = serve api (authServer :<|> gatewayServer :<|> swaggerServer)
  where
    api = Proxy :: Proxy API
    authServer = genericServer routes
    gatewayServer = genericServer gates
    swaggerServer = return swaggerData
    todoApi = genericApi (Proxy :: Proxy Routes)
    swaggerData = toSwagger todoApi
      & info.title .~ "Async Architect Workshop DB API"

main :: IO ()
main = do
  run 8080 app
