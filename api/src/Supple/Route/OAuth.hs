{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.OAuth
  ( OAuthAPI
  , oauthHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant
import Supple.AppM (AppM)
import Supple.Data.Api (OkResponse(..))
import qualified Supple.Logger as L
import Supple.Route.Util (internalServerErr)
import Supple.ShopifyClient (exchangeAccessToken)

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
      permCode <- liftIO $ exchangeAccessToken c
      L.info $ "Got code: " <> permCode
      return $ OkResponse { message = "Authenticated app" }
    Nothing -> throwError internalServerErr

oauthHandler = redirectHandler
