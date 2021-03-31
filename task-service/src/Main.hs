{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import qualified DB as DB

import Servant
import Servant.API.Generic (genericApi, ToServantApi, (:-))
import Servant.Server.Generic (genericServer, AsServer)
import Network.Wai.Handler.Warp (run)
import Data.Swagger (Swagger, title, info)
import Servant.Swagger (toSwagger)
import Control.Lens ((.~), (&))
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Network.Wai.Middleware.Cors

data Routes route = Routes
  { _tasks :: route :- "tasks" :> Get '[JSON] [Task]
  , _add :: route :- "add" :> ReqBody '[JSON] TaskData :> Post '[JSON] Int64
  , _close :: route :- "close" :> ReqBody '[JSON] Int64 :> Post '[JSON] Status
  , _reassignSingleTask :: route :- "reassign-one" :> ReqBody '[JSON] Int64 :> Post '[JSON] Status
  , _reassign :: route :- "reassign" :> Get '[JSON] Status
  } deriving (Generic)

routes :: Routes AsServer
routes = Routes
  { _tasks = DB.getTasks
  , _add = DB.addTask
  , _close = DB.closeTask
  , _reassignSingleTask = DB.reassign -- test route, unused
  , _reassign = DB.reassignTasks -- test route, unused
  }

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type API = ToServantApi Routes :<|> SwaggerAPI

app :: Application
app = simpleCors $ serve api (todoServer :<|> swaggerServer)
  where
    api = Proxy :: Proxy API
    todoServer = genericServer routes
    swaggerServer = return swaggerData
    todoApi = genericApi (Proxy :: Proxy Routes)
    swaggerData = toSwagger todoApi
      & info.title .~ "Async Architect Workshop API: Task Service"

main :: IO ()
main = do
  run 8090 app
