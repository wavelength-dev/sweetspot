module SweetSpot.Route.Webhook
  ( OrderRoute,
    RedactShopRoute,
    RedactCustomerRoute,
    RequestDataRoute,
    WebhookAPI,
    webhookHandler,
  )
where

import RIO
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api (OkResponse (..))
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import qualified SweetSpot.Logger as L
import SweetSpot.Shopify.Types

type OrderRoute =
  "webhook" :> "order"
    :> ReqBody '[JSON] Order
    :> Post '[JSON] OkResponse

type RedactShopRoute =
  "webhook" :> "redact" :> "shop"
    :> ReqBody '[JSON] RedactShop
    :> Post '[JSON] OkResponse

type RedactCustomerRoute =
  "webhook" :> "redact" :> "customer"
    :> ReqBody '[JSON] RedactCustomer
    :> Post '[JSON] OkResponse

type RequestDataRoute =
  "webhook" :> "request" :> "data"
    :> ReqBody '[JSON] RequestData
    :> Post '[JSON] OkResponse

type WebhookAPI =
  OrderRoute
    :<|> RedactShopRoute
    :<|> RedactCustomerRoute
    :<|> RequestDataRoute

orderHandler :: Order -> ServerM OkResponse
orderHandler order = runAppM $ do
  result <- validateUserCartToken $ order ^. orderCartToken
  case result of
    Just (shopId, cmpId, userId) -> do
      insertOrder shopId cmpId userId order
      return OkResponse {message = "Registered order"}
    Nothing -> return OkResponse {message = ""}

redactShopHandler :: RedactShop -> ServerM OkResponse
redactShopHandler payload = runAppM $ do
  L.info $ "**Action required** " <> tshow payload
  return OkResponse {message = "Request received"}

redactCustomerHandler :: RedactCustomer -> ServerM OkResponse
redactCustomerHandler payload = runAppM $ do
  L.info $ "**Action required** " <> tshow payload
  return OkResponse {message = "Request received"}

requestDataHandler :: RequestData -> ServerM OkResponse
requestDataHandler payload = runAppM $ do
  L.info $ "**Action required** " <> tshow payload
  return OkResponse {message = "Request received"}

webhookHandler =
  orderHandler
    :<|> redactShopHandler
    :<|> redactCustomerHandler
    :<|> requestDataHandler
