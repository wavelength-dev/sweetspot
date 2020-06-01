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
import SweetSpot.Database
  ( DbConfig (..),
    getDbPool,
    migrate,
    verifyDbSchema,
  )
import qualified SweetSpot.Env as Env
import qualified SweetSpot.Logger as L
import SweetSpot.Middleware (getMiddleware)
import SweetSpot.Route.AppCharge (AppChargeAPI, appChargeHandler)
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
           :<|> AppChargeAPI
       )
    :<|> DashboardApp
    :<|> HealthAPI
    :<|> FulcrumApp

rootAPI :: Proxy RootAPI
rootAPI = Proxy

server =
  ( fulcrumHandler
      :<|> dashboardHandler
      :<|> oauthHandler
      :<|> webhookHandler
      :<|> appChargeHandler
  )
    :<|> dashboardAppHandler
    :<|> healthHandler
    :<|> fulcrumAppHandler

createApp :: AppCtx -> Application
createApp ctx =
  getMiddleware ctx
    $ serve rootAPI
    $ hoistServer rootAPI (`runReaderT` ctx) server

runServer :: IO ()
runServer = do
  mEnvConfig <- Env.getEnvConfig
  let envConfig = either error id mEnvConfig
      dbConfig =
        DbConfig
          { host = Env.dbHost envConfig,
            name = Env.dbName envConfig,
            password = Env.dbPassword envConfig
          }
      config =
        AppConfig
          { _configEnvironment = Env.environment envConfig,
            _configShopifyClientId = Env.shopifyClientId envConfig,
            _configShopifyClientSecret = Env.shopifyClientSecret envConfig,
            _configShopifyOAuthRedirectUri =
              Env.shopifyOAuthRedirectUri envConfig
          }
  dbPool <- getDbPool dbConfig
  appLogger <- newStdoutLoggerSet defaultBufSize
  withResource dbPool migrate
  withResource dbPool verifyDbSchema
  let ctx =
        AppCtx
          { _ctxConfig = config,
            _ctxLogger = appLogger,
            _ctxDbPool = dbPool
          }
      stdoutLogger aplogger = do
        let port = read (T.unpack $ Env.port envConfig)
            app = createApp ctx
            settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings app
  L.info' appLogger ("Listening on port " <> Env.port envConfig <> "...")
  withStdoutLogger stdoutLogger
