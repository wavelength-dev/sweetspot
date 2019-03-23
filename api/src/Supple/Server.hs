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
import Network.Wai.Middleware.Gzip (def, gzip, gzipFiles, GzipFiles(GzipCacheFolder))
import Network.Wai.Middleware.Routed (routedMiddleware)
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Database (DbConfig(..), getDbPool)
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


-- WAI doesn't seem to want to know about routing.
-- This should probably move into a Servant handler somehow.
gzipStatic :: Middleware
gzipStatic = routedMiddleware ("static" `elem`) (gzip settings)
  where settings = def { gzipFiles = GzipCacheFolder "../dist/" }

createApp :: AppCtx -> Application
createApp ctx =
  corsMiddleware $
  gzipStatic $ serve rootAPI $ hoistServer rootAPI (`runReaderT` ctx) server
  where
    corsMiddleware :: Middleware
    corsMiddleware =
      cors $ \_ ->
        Just $
        simpleCorsResourcePolicy
          { corsOrigins =
              Just
                ( ["https://kamikoto.com", "https://libertyprice.myshopify.com", "http://localhost:8082"]
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
  dbPool <- getDbPool dbConfig
  appLogger <- newStdoutLoggerSet defaultBufSize
  let config = AppConfig "dev" "0.1"
      ctx = AppCtx config appLogger dbPool
  L.info' appLogger "Listening on port 8082..."
  Warp.run 8082 $ createApp ctx
