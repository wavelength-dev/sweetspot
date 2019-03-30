{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Server
  ( runServer
  ) where

import Control.Monad.Reader (runReaderT)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Database (DbConfig(..), getDbPool)
import Supple.Env (EnvConfig(..), getEnvConfig)
import qualified Supple.Logger as L
import Supple.Middleware (appMiddleware)
import Supple.Route.Dashboard (DashboardAPI, dashboardHandler)
import Supple.Route.Health (HealthAPI, healthHandler)
import Supple.Route.Injectable (InjectableAPI, injectableHandler)
import Supple.Route.Static (StaticAPI, staticHandler)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

type RootAPI
   = "api" :> (DashboardAPI :<|> InjectableAPI) :<|> HealthAPI :<|> StaticAPI

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server :: ServerT RootAPI AppM
server =
  (dashboardHandler :<|> injectableHandler) :<|> healthHandler :<|>
  staticHandler

createApp :: AppCtx -> Application
createApp ctx =
  appMiddleware $
  serve rootAPI $ hoistServer rootAPI (`runReaderT` ctx) server

runServer :: IO ()
runServer = do
  result <- getEnvConfig
  envConfig <-
    case result of
      Left msg -> error msg
      Right envConfig' -> return envConfig'
  let dbConfig =
        DbConfig
          { host = dbHost envConfig
          , name = dbName envConfig
          , password = dbPassword envConfig
          , port = dbPort envConfig
          , user = dbUser envConfig
          }
  dbPool <- getDbPool dbConfig
  appLogger <- newStdoutLoggerSet defaultBufSize
  let config = AppConfig "dev" "0.1"
      ctx = AppCtx config appLogger dbPool
  L.info' appLogger "Listening on port 8082..."
  Warp.run 8082 $ createApp ctx
