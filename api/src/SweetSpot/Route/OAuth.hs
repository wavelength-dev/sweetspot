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
  , HMAC'
  , Timestamp
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
import           SweetSpot.Database.Queries.Install
                                                ( InstallDB(..) )
import qualified SweetSpot.Logger              as L
import           SweetSpot.Route.Util
import           SweetSpot.Shopify.Client       ( MonadShopify(..) )


type InstallRoute = "oauth" :> "install"
  :> QueryParam "shop" ShopDomain
  :> QueryParam "timestamp" Timestamp
  :> QueryParam "hmac" HMAC'
  :> Get303 '[PlainText] NoContent

type RedirectRoute = "oauth" :> "redirect"
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
  "http://" <> showText shopDomain <> "/admin/oauth/authorize?"
    <> "client_id=" <> clientId
    <> "&scope=" <> scopes
    <> "&redirect_uri=" <> redirectUri
    <> "&state=" <> showText nonce
  where
    scopes = "write_products,read_orders,read_analytics"

installHandler
  :: Maybe ShopDomain
  -> Maybe Timestamp
  -> Maybe HMAC'
  -> ServerM (Headers '[Header "Location" Text] NoContent)
installHandler (Just shopDomain) (Just _) (Just hmac) = runAppM $ do
  config <- asks _getConfig
  nonce <- generateInstallNonce shopDomain
  let
    clientId = shopifyClientId config
    redirectUri = shopifyOAuthRedirectUri config
    authUri =  getAuthUri shopDomain clientId redirectUri nonce
  return $ addHeader authUri NoContent

installHandler _ _ _ = throwError badRequestErr

redirectHandler
  :: Maybe Code
  -> Maybe HMAC'
  -> Maybe Timestamp
  -> Maybe Nonce
  -> Maybe ShopDomain
  -> ServerM OkResponse
redirectHandler (Just (Code code)) (Just hmac) (Just _) (Just nonce) (Just shopDomain) =
  runAppM $ do
      mDbNonce <- getInstallNonce shopDomain
      case (== nonce) <$> mDbNonce of
        Just True -> do
          mPermCode <- exchangeAccessToken shopDomain code
          case mPermCode of
            Right permCode -> do
              deleteInstallNonce shopDomain
              createShop shopDomain permCode
              L.info $ "Successfully installed app for " <> showText shopDomain
              return OkResponse { message = "This should redirect to dashboard" }
            Left err -> do
              L.error err
              throwError internalServerErr
        _ -> do
          L.error "OAuth redirect handler got invalid nonce"
          throwError badRequestErr

redirectHandler _ _ _ _ _ = throwError badRequestErr

oauthHandler = installHandler :<|> redirectHandler
