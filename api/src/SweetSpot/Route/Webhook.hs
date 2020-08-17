module SweetSpot.Route.Webhook
  ( OrderRoute,
    AppUninstalledRoute,
    RedactShopRoute,
    RedactCustomerRoute,
    RequestDataRoute,
    WebhookAPI,
    webhookHandler,
  )
where

import Data.Aeson (toJSON)
import RIO
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api (OkResponse (..))
import SweetSpot.Data.Common (ActionRequestType (..))
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import SweetSpot.Database.Queries.Webhook (WebhookDB (..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr)
import SweetSpot.Shopify.Types

type OrderRoute =
  "webhook" :> "order"
    :> ReqBody '[JSON] Order
    :> Post '[JSON] OkResponse

type AppUninstalledRoute =
  "webhook" :> "app" :> "uninstalled"
    :> ReqBody '[JSON] AppUninstalledReq
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
    :<|> AppUninstalledRoute
    :<|> RedactShopRoute
    :<|> RedactCustomerRoute
    :<|> RequestDataRoute

orderHandler :: Order -> ServerM OkResponse
orderHandler order = runAppM $ do
  result <- validateUserCartToken $ order ^. orderCartToken
  case result of
    Just (shopId, cmpId, userId) -> do
      insertOrder shopId cmpId userId order
      L.info $ "Registered order " <> tshow shopId <> " " <> tshow cmpId <> " " <> tshow userId
      return OkResponse {message = "Registered order"}
    Nothing -> do
      L.info $ "Unable to validate cart token " <> tshow order
      return OkResponse {message = "Registered order"}

appUninstalledHandler :: AppUninstalledReq -> ServerM OkResponse
appUninstalledHandler (AppUninstalledReq domain) =
  runAppM $
    uninstallShop domain >> return OkResponse {message = "Shop successfully uninstalled"}

redactShopHandler :: RedactShop -> ServerM OkResponse
redactShopHandler payload =
  runAppM $
    insertActionRequest
      (payload ^. redactShopDomain)
      RedactShopType
      (toJSON payload)
      >> return OkResponse {message = "Request received"}

redactCustomerHandler :: RedactCustomer -> ServerM OkResponse
redactCustomerHandler payload =
  runAppM $
    insertActionRequest
      (payload ^. redactCustomerShopDomain)
      RedactCustomerType
      (toJSON payload)
      >> return OkResponse {message = "Request received"}

requestDataHandler :: RequestData -> ServerM OkResponse
requestDataHandler payload =
  runAppM $
    insertActionRequest
      (payload ^. dataRequestShopDomain)
      DataRequestType
      (toJSON payload)
      >> return OkResponse {message = "Request received"}

webhookHandler =
  orderHandler
    :<|> appUninstalledHandler
    :<|> redactShopHandler
    :<|> redactCustomerHandler
    :<|> requestDataHandler
