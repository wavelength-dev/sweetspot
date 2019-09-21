{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Route.OAuth
  ( OAuthAPI
  , oauthHandler
  )
where

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader.Class     ( asks, MonadReader(..) )
import           Data.Text                      ( Text )
import           Servant
import           SweetSpot.AppM                 ( AppM(..), ServerM )
import           SweetSpot.Data.Api             ( OkResponse(..) )
import qualified SweetSpot.Logger              as L
import           SweetSpot.Route.Util           ( internalServerErr )
import           SweetSpot.ShopifyClient        ( MonadShopify, exchangeAccessToken )

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

type OAuthAPI = "oauth" :> "redirect"
  :> QueryParam "code" Text
  :> QueryParam "hmac" Text
  :> QueryParam "timestamp" Text
  :> QueryParam "state" Text
  :> QueryParam "shop" Text
  :> Get '[JSON] OkResponse

redirectHandler
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ServerM OkResponse
redirectHandler code hmac timestamp state shop = runAppM $ case code of
  Just c -> do
    permCode <- exchangeAccessToken c
    L.info $ "Got code: " <> permCode
    return $ OkResponse { message = "Authenticated app" }
  Nothing -> throwError internalServerErr

oauthHandler = redirectHandler
