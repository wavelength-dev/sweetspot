{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Server
  ( runServer
  , rootAPI
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Database (DbConfig(..), getDbPool, migrate)
import qualified Supple.Env as Env
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
  result <- Env.getEnvConfig
  envConfig <-
    case result of
      Left msg -> error msg
      Right envConfig' -> return envConfig'
  let dbConfig =
        DbConfig
          { host = Env.dbHost envConfig
          , name = Env.dbName envConfig
          , password = Env.dbPassword envConfig
          , port = Env.dbPort envConfig
          , user = Env.dbUser envConfig
          }
  dbPool <- getDbPool dbConfig
  appLogger <- newStdoutLoggerSet defaultBufSize
  let
    config = AppConfig
      { environment = Env.environment envConfig
      , shopifyApiRoot = Env.shopifyApiRoot envConfig
      , shopifyClientId = Env.shopifyClientId envConfig
      , shopifyClientSecret = Env.shopifyClientSecret envConfig
      , shopifyOAuthAccessToken = Env.shopifyOAuthAccessToken envConfig
      }
    ctx = AppCtx
      { _getConfig = config
      , _getLogger = appLogger
      , _getDbPool = dbPool
      }

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
