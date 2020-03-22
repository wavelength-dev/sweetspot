{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Route.Webhook
  ( CheckoutRoute
  , WebhookAPI
  , webhookHandler
  ) where

import Data.Aeson (Value)
import qualified Data.Text as T
import Servant
import SweetSpot.AppM (ServerM, AppM(..))
import SweetSpot.Data.Api (OkResponse(..))
import qualified SweetSpot.Logger as L

type CheckoutRoute = "webhook" :> "checkout"
  :> ReqBody '[JSON] Value
  :> Post '[JSON] OkResponse

type WebhookAPI = CheckoutRoute

checkoutHandler :: Value -> ServerM OkResponse
checkoutHandler v = runAppM $ do
  L.info . T.pack . show $ v
  return OkResponse { message = "Received checkout" }

webhookHandler = checkoutHandler
