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
import SweetSpot.Database.Queries.Install
  ( InstallDB (..),
  )
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util
import SweetSpot.Shopify.Client (MonadShopify (..))

type InstallRoute =
  "oauth" :> "install"
    :> QueryParam "shop" ShopDomain
    :> QueryParam "timestamp" Timestamp
    :> QueryParam "hmac" HMAC'
    :> Get303 '[PlainText] NoContent

type RedirectRoute =
  "oauth" :> "redirect"
    :> QueryParam "code" Code
    :> QueryParam "hmac" HMAC'
    :> QueryParam "timestamp" Timestamp
    :> QueryParam "state" Nonce
    :> QueryParam "shop" ShopDomain
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
    scopes = "write_products,read_orders,read_analytics"

installHandler ::
  Maybe ShopDomain ->
  Maybe Timestamp ->
  Maybe HMAC' ->
  ServerM (Headers '[Header "Location" Text] NoContent)
installHandler (Just shopDomain) (Just _) (Just hmac) = runAppM $ do
  config <- asks (^. ctxConfig)
  nonce <- generateInstallNonce shopDomain
  let clientId = config ^. configShopifyClientId
      redirectUri = config ^. configShopifyOAuthRedirectUri
      authUri = getAuthUri shopDomain clientId redirectUri nonce
  return $ addHeader authUri NoContent
installHandler _ _ _ = throwError badRequestErr

redirectHandler ::
  Maybe Code ->
  Maybe HMAC' ->
  Maybe Timestamp ->
  Maybe Nonce ->
  Maybe ShopDomain ->
  ServerM (Headers '[Header "Location" Text] NoContent)
redirectHandler (Just (Code code)) (Just hmac) (Just _) (Just nonce) (Just shopDomain) =
  runAppM $ do
    mDbNonce <- getInstallNonce shopDomain
    case (== nonce) <$> mDbNonce of
      Just True -> do
        result <- runExceptT $ do
          permCode <- ExceptT $ exchangeAccessToken shopDomain code
          shopInfo <- ExceptT $ fetchShopInfo permCode shopDomain
          lift $ createShop shopDomain shopInfo permCode
          ExceptT $ registerWebhooks shopDomain
          charge <- ExceptT $ createAppCharge shopDomain
          lift $ insertAppCharge shopDomain charge
        case result of
          Left err -> do
            L.error $ err
            deleteInstallNonce shopDomain
            throwError internalServerErr
          Right _ -> do
            L.info $ "Successfully installed app for " <> showText shopDomain
            pure $ addHeader adminUrl NoContent
      _ -> do
        L.error "OAuth redirect handler got invalid nonce"
        throwError badRequestErr
  where
    adminUrl = "https://" <> showText shopDomain <> "/admin"
redirectHandler _ _ _ _ _ = throwError badRequestErr

oauthHandler = installHandler :<|> redirectHandler
