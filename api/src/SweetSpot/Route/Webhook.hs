module SweetSpot.Route.Webhook
  ( OrderRoute,
    AppUninstalledRoute,
    WebhookAPI,
    webhookHandler,
  )
where

import RIO
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api (OkResponse (..))
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import SweetSpot.Database.Queries.Webhook (WebhookDB (..))
import SweetSpot.Shopify.Types

type OrderRoute =
  "webhook" :> "order"
    :> ReqBody '[JSON] Order
    :> Post '[JSON] OkResponse

type AppUninstalledRoute =
  "webhook" :> "app" :> "uninstalled"
    :> ReqBody '[JSON] AppUninstalledReq
    :> Post '[JSON] OkResponse

type WebhookAPI = OrderRoute :<|> AppUninstalledRoute

orderHandler :: Order -> ServerM OkResponse
orderHandler order = runAppM $ do
  result <- validateUserCartToken $ order ^. orderCartToken
  case result of
    Just (shopId, cmpId, userId) -> do
      insertOrder shopId cmpId userId order
      return OkResponse {message = "Registered order"}
    Nothing -> return OkResponse {message = ""}

appUninstalledHandler :: AppUninstalledReq -> ServerM OkResponse
appUninstalledHandler (AppUninstalledReq domain) =
  runAppM $
    uninstallShop domain >> return OkResponse {message = "Shop successfully uninstalled"}

webhookHandler = orderHandler :<|> appUninstalledHandler
