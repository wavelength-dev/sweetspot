module SweetSpot.Route.DashboardApp
  ( DashboardApp,
    dashboardAppHandler,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as TE
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
import SweetSpot.Route.OAuth (InstallRoute, OAuthAPI)
import SweetSpot.Route.Util (badRequestErr)
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
    :> QueryParam "shop" ShopDomain
    :> QueryParam "timestamp" Timestamp
    :> QueryParam "hmac" HMAC'
    :> QueryParam "session" SessionId
    :> Get '[HTML] RawHTML

type DashboardStatic = "dashboard" :> Raw

type DashboardApp = IndexRoute :<|> DashboardStatic

indexHandler ::
  Maybe ShopDomain ->
  Maybe Timestamp ->
  Maybe HMAC' ->
  Maybe SessionId ->
  ServerM RawHTML
indexHandler (Just domain) (Just ts) (Just hmac) (Just sessionId) =
  runAppM $ do
    mToken <- getOAuthToken domain
    let ShopDomain txtDomain = domain
    case mToken of
      Just _ -> do
        createSession domain sessionId
        RawHTML <$> liftIO (BS.readFile "./dist/dashboard/index.html")
      Nothing -> throwError $ err302 {errHeaders = [("Location", "/api/" <> redirectPath)]}
        where
          redirectApi = Proxy :: Proxy OAuthAPI
          redirectHandler = Proxy :: Proxy InstallRoute
          redirectPath =
            TE.encodeUtf8
              $ toUrlPiece
              $ safeLink redirectApi redirectHandler (Just domain) (Just ts) (Just hmac)
indexHandler _ _ _ _ = throwError badRequestErr

dashboardStaticHandler =
  serveDirectoryWith
    defaultOptions
      { ssMaxAge = MaxAgeSeconds 600
      }
  where
    defaultOptions = defaultWebAppSettings "./dist/dashboard"

dashboardAppHandler = indexHandler :<|> dashboardStaticHandler
