{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Server
  ( runServer
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Database (DbConfig(..), getDbPool, migrate)
import Supple.Env as Env (EnvConfig(..), getEnvConfig)
import qualified Supple.Logger as L
import Supple.Middleware (getMiddleware)
import Supple.Route.Dashboard (DashboardAPI, dashboardHandler)
import Supple.Route.Health (HealthAPI, healthHandler)
import Supple.Route.Injectable (InjectableAPI, injectableHandler)
import Supple.Route.Static (StaticAPI, staticHandler)
import Supple.Route.OAuth (OAuthAPI, oauthHandler)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import System.Exit (exitWith, ExitCode(..))

type RootAPI
   = "api" :> (DashboardAPI :<|> InjectableAPI :<|> OAuthAPI) :<|> HealthAPI :<|> StaticAPI

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server :: ServerT RootAPI AppM
server =
  (dashboardHandler :<|> injectableHandler :<|> oauthHandler) :<|> healthHandler :<|>
  staticHandler

createApp :: AppCtx -> Application
createApp ctx =
  getMiddleware ctx $
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
  let env = T.pack (Env.environment envConfig)
      config = AppConfig env "0.1"
      ctx = AppCtx config appLogger dbPool

  L.info' appLogger "Running migrations"
  res <- migrate dbPool

  case res of
    Left err -> do
      L.error' appLogger ("Error while trying to migrate: " <> err)
      -- Make sure error gets logged
      threadDelay 1000000
      exitWith $ ExitFailure 1

    Right _ -> do
      L.info' appLogger "Listening on port 8082..."
      Warp.run 8082 $ createApp ctx
