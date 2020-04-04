module SweetSpot.Route.Webhook
  ( OrderRoute,
    WebhookAPI,
    webhookHandler,
  )
where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lens (packed)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api (OkResponse (..))
import SweetSpot.Data.Common (CartToken (..), Svid (..))
import SweetSpot.Database.Queries.Fulcrum (FulcrumDB (..))
import qualified SweetSpot.Logger as L
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
    Nothing -> do
      L.error $ "Failed to register order"
      return OkResponse {message = ""}

webhookHandler = orderHandler
