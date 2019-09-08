{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Server
  ( runServer
  , rootAPI
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import Data.Pool (withResource)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings, setLogger)
import Servant
import SweetSpot.AppM (AppConfig(..), AppCtx(..), AppM)
import SweetSpot.Database (DbConfig(..), getDbPool, migrate)
import qualified SweetSpot.Env as Env
import qualified SweetSpot.Logger as L
import SweetSpot.Middleware (getMiddleware)
import SweetSpot.Route.Dashboard (DashboardAPI, dashboardHandler)
import SweetSpot.Route.Health (HealthAPI, healthHandler)
import SweetSpot.Route.Injectable (InjectableAPI, injectableHandler)
import SweetSpot.Route.Static (StaticAPI, staticHandler)
import SweetSpot.Route.OAuth (OAuthAPI, oauthHandler)
import SweetSpot.Shop as Shop
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
  shop <- case Shop.readShop (Env.targetShop envConfig) of
            Left msg -> error msg
            Right shop -> return shop
  let
    shopConfig = getShopConfig shop
    config = AppConfig
      { environment = Env.environment envConfig
      , shopifyApiRoot = Shop.shopApi shopConfig
      , shopifyAccessTokenEndpoint = Shop.accessTokenEndpoint shopConfig
      , shopifyClientId = Shop.clientId shopConfig
      , shopifyClientSecret = Env.shopifyClientSecret envConfig
      , shopifyOAuthAccessToken = Env.shopifyOAuthAccessToken envConfig
      }
    ctx = AppCtx
      { _getConfig = config
      , _getLogger = appLogger
      , _getDbPool = dbPool
      }

  L.info' appLogger "Running migrations"
  res <- withResource dbPool $ \conn -> migrate conn

  case res of
    Nothing -> do
      L.error' appLogger "Error while trying to migrate"
      -- Make sure error gets logged
      threadDelay 1000000
      exitWith $ ExitFailure 1

    Just _ -> do
      L.info' appLogger "Listening on port 8082..."
      withStdoutLogger $ \aplogger -> do
        let settings = setPort 8082 $ setLogger aplogger defaultSettings
        runSettings settings $ createApp ctx
