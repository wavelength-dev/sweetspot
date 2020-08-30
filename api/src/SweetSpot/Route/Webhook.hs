module SweetSpot.Route.Webhook
  ( AppUninstalledRoute,
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
import SweetSpot.Database.Queries.Webhook (WebhookDB (..))
import SweetSpot.Shopify.Types

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
  AppUninstalledRoute
    :<|> RedactShopRoute
    :<|> RedactCustomerRoute
    :<|> RequestDataRoute

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
  appUninstalledHandler
    :<|> redactShopHandler
    :<|> redactCustomerHandler
    :<|> requestDataHandler
