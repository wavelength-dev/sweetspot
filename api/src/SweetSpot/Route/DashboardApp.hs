module SweetSpot.Route.DashboardApp
  ( DashboardApp,
    dashboardAppHandler,
  )
where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Media ((//), (/:))
import RIO
import qualified RIO.ByteString.Lazy as BSL
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
import SweetSpot.Shopify.Client (MonadShopify (..))
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (MaxAge (..), ssMaxAge)

data HTML

newtype RawHTML = RawHTML {unRaw :: BSL.ByteString}

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

parentWindowRedirect :: Text -> BSL.ByteString
parentWindowRedirect url =
  "<html><head><meta charset=\"utf=8\"><title>Redirecting...</title><script>"
    <> "window.top.location.href = "
    <> "\""
    <> BSL.fromStrict (encodeUtf8 url)
    <> "\""
    <> "</script></head><body></body></html>"

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
        oldAppCharge <- getAppCharge domain
        eAppChargeRes <- fetchAppChargeStatus domain (oldAppCharge ^. appChargeShopifyId)
        case eAppChargeRes of
          Left err -> do
            L.error $ "Failed to fetch app charge " <> err
            throwError err500
          Right appChargeRes -> do
            deleteAppCharge domain
            insertAppCharge domain appChargeRes
            appCharge <- getAppCharge domain
            case _appChargeStatus appCharge of
              Active -> RawHTML <$> liftIO (BSL.readFile "./dist/dashboard/index.html")
              Declined -> do
                deleteAppCharge domain
                charge <- createAppCharge domain
                case charge of
                  Left err -> do
                    L.error $ "Failed to create app charge " <> err
                    throwError err500
                  Right chargeRes -> do
                    appCharge <- insertAppCharge domain chargeRes
                    pure $ RawHTML $ parentWindowRedirect (appCharge ^. appChargeConfirmationUrl)
              status -> do
                L.warn $ "Shop " <> showText domain <> " no active appCharge: " <> tshow status
                pure $ RawHTML $ parentWindowRedirect $ _appChargeConfirmationUrl appCharge
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
