module SweetSpot.Route.Webhook
  ( CheckoutRoute,
    WebhookAPI,
    webhookHandler,
  )
where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Lens (packed)
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api (OkResponse (..))
import SweetSpot.Data.Common (Svid (..))
import qualified SweetSpot.Logger as L

type CheckoutRoute =
  "webhook" :> "checkout"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] OkResponse

type WebhookAPI = CheckoutRoute

checkoutHandler :: Value -> ServerM OkResponse
checkoutHandler v = runAppM $ do
  let conversions :: [(Svid, Integer)]
      conversions =
        v ^.. key "line_items"
          . values
          . to
            ( \li ->
                liftA2
                  (,)
                  (li ^? key "variant_id" . _Integer . to show . packed . to Svid)
                  (li ^? key "quantity" . _Integer)
            )
          & catMaybes
  L.info . T.pack . show $ conversions
  return OkResponse {message = "Received checkout"}

webhookHandler = checkoutHandler
