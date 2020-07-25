module SweetSpot.Route.DashboardApp
  ( DashboardApp,
    dashboardAppHandler,
  )
where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Media ((//), (/:))
import RIO
import qualified RIO.ByteString.Lazy as BS
import Servant
import Servant ((:>), Raw, serveDirectoryWith)
import Servant.API (toUrlPiece)
import Servant.Links (safeLink)
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Dashboard (DashboardDB (..))
import SweetSpot.Database.Queries.Install (InstallDB (..))
import SweetSpot.Database.Schema
import qualified SweetSpot.Logger as L
import SweetSpot.Route.OAuth (InstallRoute, OAuthAPI)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge)

data HTML

newtype RawHTML = RawHTML {unRaw :: BS.ByteString}

instance MimeRender HTML RawHTML where
  mimeRender _ = unRaw

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

type IndexRoute =
  "dashboard" :> "index.html"
    :> QueryParam' '[Required, Strict] "shop" ShopDomain
    :> QueryParam' '[Required, Strict] "timestamp" Timestamp
    :> QueryParam' '[Required, Strict] "hmac" HMAC'
    :> QueryParam "session" SessionId
    :> Get '[HTML] RawHTML

type DashboardStatic = "dashboard" :> Raw

type DashboardApp = IndexRoute :<|> DashboardStatic

indexHandler ::
  ShopDomain ->
  Timestamp ->
  HMAC' ->
  Maybe SessionId ->
  ServerM RawHTML
indexHandler domain ts hmac mSessionId =
  runAppM $ do
    mToken <- getOAuthToken domain
    case (mToken, mSessionId) of
      (Just _, Just sessionId) -> do
        createSession domain sessionId
        appCharge <- getAppCharge domain
        case _appChargeStatus appCharge of
          Active -> RawHTML <$> liftIO (BS.readFile "./dist/dashboard/index.html")
          status -> do
            L.warn $ "Shop " <> showText domain <> " no active appCharge: " <> tshow status
            throwError $
              err302
                { errHeaders =
                    [("Location", encodeUtf8 (_appChargeConfirmationUrl appCharge))]
                }
      _ -> throwError $ err302 {errHeaders = [("Location", "/api/" <> installPath)]}
        where
          redirectApi = Proxy :: Proxy OAuthAPI
          redirectHandler = Proxy :: Proxy InstallRoute
          installPath =
            encodeUtf8
              $ toUrlPiece
              $ safeLink redirectApi redirectHandler domain ts hmac

dashboardStaticHandler =
  serveDirectoryWith
    defaultOptions
      { ssMaxAge = MaxAgeSeconds 600
      }
  where
    defaultOptions = defaultWebAppSettings "./dist/dashboard"

dashboardAppHandler = indexHandler :<|> dashboardStaticHandler
