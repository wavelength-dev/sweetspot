{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Server
    ( runServer
    , rootAPI
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (runReaderT)
import Data.Pool (withResource)
import Data.Text as T
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setLogger
    , setPort
    )
import Network.Wai.Logger (withStdoutLogger)
import Servant hiding (basicAuthPassword)
import SweetSpot.AppM
import SweetSpot.Database (DbConfig(..), getDbPool, migrate)
import qualified SweetSpot.Env as Env
import qualified SweetSpot.Logger as L
import SweetSpot.Middleware (getMiddleware)
import SweetSpot.Route.Dashboard (DashboardAPI, dashboardHandler)
import SweetSpot.Route.Health (HealthAPI, healthHandler)
import SweetSpot.Route.DashboardApp (DashboardApp, dashboardAppHandler)
import SweetSpot.Route.Injectable (InjectableAPI, injectableHandler)
import SweetSpot.Route.OAuth (OAuthAPI, oauthHandler)
import SweetSpot.Route.FulcrumApp (FulcrumApp, fulcrumAppHandler)
import SweetSpot.Route.Webhook (WebhookAPI, webhookHandler)
import System.Exit (ExitCode(..), exitWith)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

type RootAPI =
    "api" :>
        (InjectableAPI
        :<|> DashboardAPI
        :<|> OAuthAPI
        :<|> WebhookAPI
        )
    :<|> DashboardApp
    :<|> HealthAPI
    :<|> FulcrumApp

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server =
    (injectableHandler :<|> dashboardHandler :<|> oauthHandler :<|> webhookHandler) :<|>
    dashboardAppHandler :<|>
    healthHandler :<|>
    fulcrumAppHandler

createApp :: AppCtx -> Application
createApp ctx =
    getMiddleware ctx $
    serve rootAPI $ hoistServer rootAPI (`runReaderT` ctx) server

rightOrThrow :: Either Text a -> a
rightOrThrow (Left msg) = error . T.unpack $ msg
rightOrThrow (Right a) = a

runServer :: IO ()
runServer = do
    mEnvConfig <- Env.getEnvConfig
    let envConfig = either error id mEnvConfig
    let dbConfig =
            DbConfig
                { host = Env.dbHost envConfig
                , name = Env.dbName envConfig
                , password = Env.dbPassword envConfig
                }
    let config =
            AppConfig
                { _configEnvironment = Env.environment envConfig
                , _configShopifyClientId = Env.shopifyClientId envConfig
                , _configShopifyClientSecret = Env.shopifyClientSecret envConfig
                , _configShopifyOAuthRedirectUri =
                      Env.shopifyOAuthRedirectUri envConfig
                }
    dbPool <- getDbPool dbConfig
    appLogger <- newStdoutLoggerSet defaultBufSize
    let ctx =
            AppCtx
                { _ctxConfig = config
                , _ctxLogger = appLogger
                , _ctxDbPool = dbPool
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
            L.info'
                appLogger
                ("Listening on port " <> Env.port envConfig <> "...")
            withStdoutLogger stdoutLogger
            where stdoutLogger aplogger = do
                      let port = read (T.unpack $ Env.port envConfig)
                      let app = createApp ctx
                      let settings =
                              setPort port $ setLogger aplogger defaultSettings
                      runSettings settings app
