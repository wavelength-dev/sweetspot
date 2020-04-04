module SweetSpot.Route.Webhook
  ( OrderRoute,
    WebhookAPI,
    webhookHandler,
  )
where

import Control.Lens
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api (OkResponse (..))
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import SweetSpot.Shopify.Types

type OrderRoute =
  "webhook" :> "order"
    :> ReqBody '[JSON] Order
    :> Post '[JSON] OkResponse

type WebhookAPI = OrderRoute

orderHandler :: Order -> ServerM OkResponse
orderHandler order = runAppM $ do
  result <- validateUserCartToken $ order ^. orderCartToken
  case result of
    Just (shopId, cmpId, userId) -> do
      insertOrder shopId cmpId userId order
      return OkResponse {message = "Registered order"}
    Nothing -> return OkResponse {message = ""}

webhookHandler = orderHandler
