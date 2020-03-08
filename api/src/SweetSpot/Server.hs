{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Server
        ( runServer
        , rootAPI
        )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Reader           ( runReaderT )
import           Data.Pool                      ( withResource )
import           Data.Text                     as T
import           Network.Wai.Logger             ( withStdoutLogger )
import           Network.Wai.Handler.Warp       ( defaultSettings
                                                , setPort
                                                , runSettings
                                                , setLogger
                                                )
import           Servant                 hiding ( basicAuthPassword )
import           SweetSpot.AppM                 ( AppConfig(..)
                                                , AppCtx(..)
                                                )
import           SweetSpot.Database             ( DbConfig(..)
                                                , getDbPool
                                                , migrate
                                                )
import qualified SweetSpot.Env                 as Env
import qualified SweetSpot.Logger              as L
import           SweetSpot.Middleware           ( getMiddleware )
import           SweetSpot.Route.Dashboard      ( DashboardAPI
                                                , dashboardHandler
                                                )
import           SweetSpot.Route.Health         ( HealthAPI
                                                , healthHandler
                                                )
import           SweetSpot.Route.Injectable     ( InjectableAPI
                                                , injectableHandler
                                                )
import           SweetSpot.Route.Static         ( StaticAPI
                                                , staticHandler
                                                )
import           SweetSpot.Route.OAuth          ( OAuthAPI
                                                , oauthHandler
                                                )
import           SweetSpot.Route.Index          ( IndexRoute
                                                , indexHandler
                                                )
import           System.Log.FastLogger          ( defaultBufSize
                                                , newStdoutLoggerSet
                                                )
import           System.Exit                    ( exitWith
                                                , ExitCode(..)
                                                )

type RootAPI
        = "api" :>
        (InjectableAPI :<|> DashboardAPI :<|> OAuthAPI)
        :<|> HealthAPI :<|> StaticAPI :<|> IndexRoute

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server = (injectableHandler :<|> dashboardHandler :<|> oauthHandler)
          :<|> healthHandler :<|> staticHandler :<|> indexHandler

createApp :: AppCtx -> Application
createApp ctx = getMiddleware ctx $ serve rootAPI $ hoistServer
        rootAPI
        (`runReaderT` ctx)
        server

rightOrThrow :: Either Text a -> a
rightOrThrow (Left  msg) = error . T.unpack $ msg
rightOrThrow (Right a  ) = a

runServer :: IO ()
runServer = do
        mEnvConfig <- Env.getEnvConfig
        let envConfig  = either error id mEnvConfig
        let dbConfig = DbConfig { host     = Env.dbHost envConfig
                                , name     = Env.dbName envConfig
                                , password = Env.dbPassword envConfig
                                }
        let
                config = AppConfig
                        { environment = Env.environment envConfig
                        , shopifyClientId = Env.shopifyClientId envConfig
                        , shopifyClientSecret = Env.shopifyClientSecret
                                                        envConfig
                        , shopifyOAuthRedirectUri = Env.shopifyOAuthRedirectUri envConfig
                        , basicAuthUser = Env.basicAuthUser envConfig
                        , basicAuthPassword = Env.basicAuthPassword envConfig
                        }
        dbPool    <- getDbPool dbConfig
        appLogger <- newStdoutLoggerSet defaultBufSize
        let ctx = AppCtx { _getConfig = config
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
                        L.info'
                                appLogger
                                (  "Listening on port "
                                <> Env.port envConfig
                                <> "..."
                                )
                        withStdoutLogger stdoutLogger
                    where
                        stdoutLogger aplogger = do
                                let port = read (T.unpack $ Env.port envConfig)
                                let app  = createApp ctx
                                let
                                        settings = setPort port $ setLogger
                                                aplogger
                                                defaultSettings
                                runSettings settings app
