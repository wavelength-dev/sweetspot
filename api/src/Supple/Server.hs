{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Server
  ( runServer
  ) where

import Configuration.Dotenv (defaultConfig, defaultValidatorMap, loadSafeFile)
import Control.Monad.Reader (runReaderT)
import Data.Default (def)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
  ( cors
  , corsExposedHeaders
  , corsRequestHeaders
  , corsMethods
  , corsOrigins
  , simpleCorsResourcePolicy
  , simpleHeaders
  , simpleMethods
  )
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Servant
import Supple.AppM (AppConfig(..), AppCtx(..), AppM)
import Supple.Database (DbConfig(..), getDbConnection)
import Supple.Route.Dashboard (DashboardAPI, dashboardHandler)
import Supple.Route.Injectable (InjectableAPI, injectableHandler)
import System.Environment (getEnv)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

type RootAPI = DashboardAPI :<|> InjectableAPI

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server :: ServerT RootAPI AppM
server = dashboardHandler :<|> injectableHandler

createApp :: AppCtx -> Application
createApp ctx =
  corsMiddleware $ serve rootAPI $ hoistServer rootAPI (flip runReaderT ctx) server
    where
    corsMiddleware :: Middleware
    corsMiddleware = cors $ \_ ->
      Just $ simpleCorsResourcePolicy
        { corsOrigins = Just (["https://libertyprice.myshopify.com"], True)
        , corsRequestHeaders = "Content-Type" : simpleHeaders
        , corsMethods = simpleMethods
        , corsExposedHeaders =
            Just ["Set-Cookie", "Access-Control-Allow-Origin", "Content-Type"]      }

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $
  def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

runServer :: IO ()
runServer = do
  loadSafeFile defaultValidatorMap ".schema.yml" defaultConfig
  host <- getEnv "DB_HOST"
  port <- getEnv "DB_PORT"
  name <- getEnv "DB_NAME"
  user <- getEnv "DB_USER"
  pass <- getEnv "DB_PASSWORD"
  let dbConfig =
        DbConfig
          { host = host
          , port = read port
          , name = name
          , user = user
          , password = pass
          }
  dbconn <- getDbConnection dbConfig
  warpLogger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize
  let config = AppConfig "dev" "0.1"
      ctx = AppCtx config appLogger dbconn
  Warp.run 8082 $ warpLogger $ createApp ctx
