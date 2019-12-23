{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Route.OAuth
  ( OAuthAPI
  , oauthHandler
  )
where

import           Control.Monad.Reader.Class     ( asks )
import           Data.Text                      ( Text )
import           Servant
import           SweetSpot.AppM                 ( AppM(..)
                                                , AppConfig(..)
                                                , AppCtx(..)
                                                , ServerM
                                                )
import           SweetSpot.Data.Common
import           SweetSpot.Data.Api             ( OkResponse(..) )
import           SweetSpot.Database.Queries.Injectable (InjectableDB(..))
import qualified SweetSpot.Logger              as L
import           SweetSpot.Route.Util
import           SweetSpot.ShopifyClient        ( exchangeAccessToken )

-- See: https://help.shopify.com/en/api/getting-started/authentication/oauth
-- The below handler handles Shopify redirecting the merchant to us, with an authorization_code is the URL. We use that authorization_code to then make a request to Shopify that gets us an access token for the given store.
-- TODO: Make the nonce a randomly selected value checked against the nonce in the oauth callback

-- LibertyPrice
-- scopes read_products,write_products,read_orders,read_analytics
-- redirect_uri https://app-staging.getsweetspot.com/api/oauth/redirect
-- nonce 1123
--
-- Longvadon
-- scopes read_products,write_products,read_orders,read_analytics
-- redirect_uri https://app.getsweetspot.com/api/oauth/redirect
-- nonce 1123


newtype Code = Code Text

instance FromHttpApiData Code where
  parseQueryParam = Right . Code

newtype HMAC' = HMAC' Text

instance FromHttpApiData HMAC' where
  parseQueryParam = Right . HMAC'

newtype Timestamp = Timestamp Text

instance FromHttpApiData Timestamp where
  parseQueryParam = Right . Timestamp

type InstallRoute = "oauth" :> "install"
  :> AllQueryParams
  :> QueryParam "shop" ShopDomain
  :> QueryParam "timestamp" Timestamp
  :> QueryParam "hmac" HMAC'
  :> Get303 '[JSON] NoContent

type RedirectRoute = "oauth" :> "redirect"
  :> AllQueryParams
  :> QueryParam "code" Code
  :> QueryParam "hmac" HMAC'
  :> QueryParam "timestamp" Timestamp
  :> QueryParam "state" Nonce
  :> QueryParam "shop" ShopDomain
  :> Get '[JSON] OkResponse

type OAuthAPI = InstallRoute :<|> RedirectRoute

getAuthUri
  :: ShopDomain
  -> Text
  -> Text
  -> Nonce
  -> Text
getAuthUri shopDomain clientId redirectUri nonce =
  "https://" <> showText shopDomain <> "/admin/oauth/authorize?"
    <> "client_id=" <> clientId
    <> "&scope=" <> scopes
    <> "&redirect_uri=" <> redirectUri
    <> "&state=" <> showText nonce
  where
    scopes = "write_products,read_orders,read_analytics"

installHandler
  :: QueryParamsList
  -> Maybe ShopDomain
  -> Maybe Timestamp
  -> Maybe HMAC'
  -> ServerM (Headers '[Header "Location" Text] NoContent)
installHandler params (Just shopDomain) (Just ts) (Just hmac) = runAppM $ do
  config <- asks _getConfig
  nonce <- generateInstallNonce shopDomain
  let
    clientId = shopifyClientId config
    redirectUri = shopifyOAuthRedirectUri config
    authUri =  getAuthUri shopDomain clientId redirectUri nonce
  return $ addHeader authUri NoContent

installHandler _ _ _ _ = throwError badRequestErr

redirectHandler
  :: QueryParamsList
  -> Maybe Code
  -> Maybe HMAC'
  -> Maybe Timestamp
  -> Maybe Nonce
  -> Maybe ShopDomain
  -> ServerM OkResponse
redirectHandler params (Just (Code code)) (Just hmac) (Just timestamp) (Just nonce) (Just shopDomain) =
  runAppM $ do
      mDbNonce <- getInstallNonce shopDomain
      case (== nonce) <$> mDbNonce of
        Just True -> do
          permCode <- exchangeAccessToken code
          deleteInstallNonce shopDomain
          createShop shopDomain permCode
          L.info $ "Successfully installed app for " <> showText shopDomain
          return OkResponse { message = "Authenticated app" }
        _ -> do
          L.error "OAuth redirect handler got invalid nonce"
          throwError badRequestErr

redirectHandler _ _ _ _ _ _ = throwError badRequestErr

oauthHandler = installHandler :<|> redirectHandler
