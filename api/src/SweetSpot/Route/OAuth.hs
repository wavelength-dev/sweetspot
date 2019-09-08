{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Route.OAuth
  ( OAuthAPI
  , oauthHandler
  )
where

import           Data.Text                      ( Text )
import           Servant
import           SweetSpot.AppM                 ( AppM )
import           SweetSpot.Data.Api             ( OkResponse(..) )
import qualified SweetSpot.Logger              as L
import           SweetSpot.Route.Util           ( internalServerErr )
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
  -> AppM OkResponse
redirectHandler code hmac timestamp state shop = case code of
  Just c -> do
    permCode <- exchangeAccessToken c
    L.info $ "Got code: " <> permCode
    return $ OkResponse { message = "Authenticated app" }
  Nothing -> throwError internalServerErr

oauthHandler = redirectHandler
