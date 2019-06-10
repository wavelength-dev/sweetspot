{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Route.OAuth
  ( OAuthAPI
  , oauthHandler
  ) where

import Data.Text (Text)
import Servant
import SweetSpot.AppM (AppM)
import SweetSpot.Data.Api (OkResponse(..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr)
import SweetSpot.ShopifyClient (exchangeAccessToken)

type RedirectRoute
   = "redirect" :> QueryParam "code" Text :> QueryParam "hmac" Text :> QueryParam "timestamp" Text :> QueryParam "state" Text :> QueryParam "shop" Text :> Get '[JSON] OkResponse

type OAuthAPI = "oauth" :> RedirectRoute

redirectHandler ::
     Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> AppM OkResponse
redirectHandler code hmac timestamp state shop = do
  case code of
    Just c -> do
      permCode <- exchangeAccessToken c
      L.info $ "Got code: " <> permCode
      return $ OkResponse { message = "Authenticated app" }
    Nothing -> throwError internalServerErr

oauthHandler = redirectHandler
