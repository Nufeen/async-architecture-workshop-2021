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

data Routes route = Routes
  {
  } deriving (Generic)

routes :: Routes AsServer
routes = Routes
  {
  }

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type API = ToServantApi Routes :<|> SwaggerAPI

app :: Application
app = serve api (todoServer :<|> swaggerServer)
  where
    api = Proxy :: Proxy API
    todoServer = genericServer routes
    swaggerServer = return swaggerData
    todoApi = genericApi (Proxy :: Proxy Routes)
    swaggerData = toSwagger todoApi
      & info.title .~ "Async Architect Workshop DB API: Transaction"

main :: IO ()
main = do
  run 8090 app
