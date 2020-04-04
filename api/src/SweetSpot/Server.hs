module SweetSpot.Server
  ( runServer,
    rootAPI,
  )
where

import Control.Monad.Reader (runReaderT)
import Data.Pool (withResource)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import RIO
import RIO.Partial (read)
import RIO.Text as T
import Servant hiding (basicAuthPassword)
import SweetSpot.AppM
import SweetSpot.Database (DbConfig (..), getDbPool, migrate)
import qualified SweetSpot.Env as Env
import qualified SweetSpot.Logger as L
import SweetSpot.Middleware (getMiddleware)
import SweetSpot.Route.Dashboard (DashboardAPI, dashboardHandler)
import SweetSpot.Route.DashboardApp (DashboardApp, dashboardAppHandler)
import SweetSpot.Route.Fulcrum (FulcrumAPI, fulcrumHandler)
import SweetSpot.Route.FulcrumApp (FulcrumApp, fulcrumAppHandler)
import SweetSpot.Route.Health (HealthAPI, healthHandler)
import SweetSpot.Route.OAuth (OAuthAPI, oauthHandler)
import SweetSpot.Route.Webhook (WebhookAPI, webhookHandler)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)

type RootAPI =
  "api"
    :> ( FulcrumAPI
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
  (fulcrumHandler :<|> dashboardHandler :<|> oauthHandler :<|> webhookHandler)
    :<|> dashboardAppHandler
    :<|> healthHandler
    :<|> fulcrumAppHandler

createApp :: AppCtx -> Application
createApp ctx =
  getMiddleware ctx
    $ serve rootAPI
    $ hoistServer rootAPI (`runReaderT` ctx) server

rightOrThrow :: Either Text a -> a
rightOrThrow (Left msg) = error . T.unpack $ msg
rightOrThrow (Right a) = a

runServer :: IO ()
runServer = do
  mEnvConfig <- Env.getEnvConfig
  let envConfig = either error id mEnvConfig
  let dbConfig =
        DbConfig
          { host = Env.dbHost envConfig,
            name = Env.dbName envConfig,
            password = Env.dbPassword envConfig
          }
  let config =
        AppConfig
          { _configEnvironment = Env.environment envConfig,
            _configShopifyClientId = Env.shopifyClientId envConfig,
            _configShopifyClientSecret = Env.shopifyClientSecret envConfig,
            _configShopifyOAuthRedirectUri =
              Env.shopifyOAuthRedirectUri envConfig
          }
  dbPool <- getDbPool dbConfig
  appLogger <- newStdoutLoggerSet defaultBufSize
  let ctx =
        AppCtx
          { _ctxConfig = config,
            _ctxLogger = appLogger,
            _ctxDbPool = dbPool
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
      L.info' appLogger ("Listening on port " <> Env.port envConfig <> "...")
      withStdoutLogger stdoutLogger
      where
        stdoutLogger aplogger = do
          let port = read (T.unpack $ Env.port envConfig)
              app = createApp ctx
              settings = setPort port $ setLogger aplogger defaultSettings
          runSettings settings app
