module SweetSpot.Route.OAuth
  ( OAuthAPI,
    InstallRoute,
    oauthHandler,
    HMAC',
    Timestamp,
  )
where

import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import RIO
import Servant
import SweetSpot.AppM
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Install (InstallDB (..))
import SweetSpot.Database.Queries.Webhook (WebhookDB (..))
import SweetSpot.Database.Schema
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util
import SweetSpot.Shopify.Client (MonadShopify (..))

type InstallRoute =
  "oauth" :> "install"
    :> QueryParam' '[Required, Strict] "shop" ShopDomain
    :> QueryParam' '[Required, Strict] "timestamp" Timestamp
    :> QueryParam' '[Required, Strict] "hmac" HMAC'
    :> Get303 '[PlainText] NoContent

type RedirectRoute =
  "oauth" :> "redirect"
    :> QueryParam' '[Required, Strict] "code" Code
    :> QueryParam' '[Required, Strict] "hmac" HMAC'
    :> QueryParam' '[Required, Strict] "timestamp" Timestamp
    :> QueryParam' '[Required, Strict] "state" Nonce
    :> QueryParam' '[Required, Strict] "shop" ShopDomain
    :> Get303 '[PlainText] NoContent

type OAuthAPI = InstallRoute :<|> RedirectRoute

getAuthUri ::
  ShopDomain ->
  Text ->
  Text ->
  Nonce ->
  Text
getAuthUri shopDomain clientId redirectUri nonce =
  "https://" <> showText shopDomain <> "/admin/oauth/authorize?"
    <> "client_id="
    <> clientId
    <> "&scope="
    <> scopes
    <> "&redirect_uri="
    <> redirectUri
    <> "&state="
    <> showText nonce
  where
    scopes = "write_products,write_script_tags,read_orders,read_analytics"

installHandler ::
  ShopDomain ->
  Timestamp ->
  HMAC' ->
  ServerM (Headers '[Header "Location" Text] NoContent)
installHandler shopDomain ts hmac = runAppM $ do
  config <- asks (^. ctxConfig)
  nonce <- generateInstallNonce shopDomain
  let clientId = config ^. configShopifyClientId
      redirectUri = config ^. configShopifyOAuthRedirectUri
      authUri = getAuthUri shopDomain clientId redirectUri nonce
  pure $ addHeader authUri NoContent

redirectHandler ::
  Code ->
  HMAC' ->
  Timestamp ->
  Nonce ->
  ShopDomain ->
  ServerM (Headers '[Header "Location" Text] NoContent)
redirectHandler (Code code) hmac _ nonce shopDomain =
  runAppM $ do
    mDbNonce <- getInstallNonce shopDomain
    case (== nonce) <$> mDbNonce of
      Just True -> do
        result <- runExceptT $ do
          permCode <- ExceptT $ exchangeAccessToken shopDomain code
          shopInfo <- ExceptT $ fetchShopInfo permCode shopDomain
          lift $ createShop shopDomain shopInfo permCode
          ExceptT $ createScript shopDomain
          ExceptT $ registerWebhooks shopDomain
          charge <- ExceptT $ createAppCharge shopDomain
          lift $ insertAppCharge shopDomain charge
        case result of
          Left err -> do
            L.error err
            deleteInstallNonce shopDomain
            uninstallShop shopDomain
            throwError internalServerErr
          Right appCharge -> do
            L.info $ "Successfully installed app for " <> showText shopDomain
            deleteInstallNonce shopDomain
            pure $ addHeader (appCharge ^. appChargeConfirmationUrl) NoContent
      _ -> do
        L.error "OAuth redirect handler got invalid nonce"
        throwError badRequestErr

oauthHandler = installHandler :<|> redirectHandler
