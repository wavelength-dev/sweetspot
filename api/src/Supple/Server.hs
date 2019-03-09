{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Server
  ( runServer
  ) where

import Control.Monad.Reader (runReaderT)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
  ( cors
  , corsExposedHeaders
  , corsMethods
  , corsOrigins
  , corsRequestHeaders
  , simpleCorsResourcePolicy
  , simpleHeaders
  , simpleMethods
  )
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Database (DbConfig(..), getDbConnection)
import Supple.Env (getEnvConfig, EnvConfig(..))
import Supple.Route.Dashboard (DashboardAPI, dashboardHandler)
import Supple.Route.Injectable (InjectableAPI, injectableHandler)
import Supple.Route.Static (StaticAPI, staticHandler)
import Supple.Route.Health (HealthAPI, healthHandler)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import qualified Supple.Logger as L

type RootAPI = "api" :> (DashboardAPI :<|> InjectableAPI) :<|> HealthAPI :<|> StaticAPI

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server :: ServerT RootAPI AppM
server = (dashboardHandler :<|> injectableHandler) :<|> healthHandler :<|> staticHandler

createApp :: AppCtx -> Application
createApp ctx =
  corsMiddleware $
  serve rootAPI $ hoistServer rootAPI (`runReaderT` ctx) server
  where
    corsMiddleware :: Middleware
    corsMiddleware =
      cors $ \_ ->
        Just $
        simpleCorsResourcePolicy
          { corsOrigins =
              Just
                ( ["https://libertyprice.myshopify.com", "http://localhost:8082"]
                , True)
          , corsRequestHeaders = "Content-Type" : simpleHeaders
          , corsMethods = simpleMethods
          , corsExposedHeaders =
              Just ["Set-Cookie", "Access-Control-Allow-Origin", "Content-Type"]
          }

runServer :: IO ()
runServer = do
  result <- getEnvConfig
  envConfig <- case result of
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
  dbconn <- getDbConnection dbConfig
  appLogger <- newStdoutLoggerSet defaultBufSize
  let config = AppConfig "dev" "0.1"
      ctx = AppCtx config appLogger dbconn
  L.info' appLogger "Listening on port 8082..."
  Warp.run 8082 $ createApp ctx
