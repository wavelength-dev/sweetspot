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
import           Crypto.Hash                    ( SHA256
                                                , Digest
                                                )
import           Crypto.MAC.HMAC
import qualified Data.ByteString               as BS
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Servant
import           SweetSpot.AppM                 ( AppM(..)
                                                , AppConfig(..)
                                                , AppCtx(..)
                                                , ServerM
                                                )
import           SweetSpot.Data.Common
import           SweetSpot.Data.Api             ( OkResponse(..) )
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

newtype State = State Text

instance FromHttpApiData State where
  parseQueryParam = Right . State

type OAuthAPI = "oauth" :> "redirect"
  :> AllQueryParams
  :> QueryParam "code" Code
  :> QueryParam "hmac" HMAC'
  :> QueryParam "timestamp" Timestamp
  :> QueryParam "state" State
  :> QueryParam "shop" ShopDomain
  :> Get '[JSON] OkResponse

checkHostname :: Text -> Bool
checkHostname = undefined

verifyRequest :: Text -> HMAC' -> QueryParamsList -> Bool
verifyRequest secret (HMAC' hmac') params = digestTxt == hmac'
  where
    filtered = mapMaybe (\(key, val) -> fmap (\v -> key <> "=" <> v) val) params
    checkable = BS.intercalate "&" filtered
    digest = hmacGetDigest $ hmac (encodeUtf8 secret) checkable :: Digest SHA256
    digestTxt = T.pack . show $ digest

redirectHandler
  :: QueryParamsList
  -> Maybe Code
  -> Maybe HMAC'
  -> Maybe Timestamp
  -> Maybe State
  -> Maybe ShopDomain
  -> ServerM OkResponse
redirectHandler params code hmac timestamp state shop = runAppM $
  case (params, code, hmac, timestamp, state, shop) of
    (params, Just (Code code), Just hmac, Just ts, Just state, Just shop) -> do
      secret <- asks (shopifyClientSecret . _getConfig)
      let isValid = verifyRequest secret hmac params
      if isValid
        then do
          permCode <- exchangeAccessToken code
          L.info $ "Got code: " <> permCode
          return OkResponse { message = "Authenticated app" }
        else throwError badRequestErr

    _ -> throwError badRequestErr

oauthHandler = redirectHandler
