module SweetSpot.SiteCapabilities.PriceControl (setControlledPrice, setControlledPriceM) where

import Prelude

import Control.Monad.Reader (ask) as Reader
import Control.Monad.Reader (class MonadAsk)
import Data.Array as Array
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import SweetSpot.Data.Config (DryRunMode(..))
import SweetSpot.Data.Config (dryRunMode, idClass) as Config
import SweetSpot.Data.Domain (Sku(..), TestContext)
import SweetSpot.Intl (formatPrice) as Intl
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node (setTextContent)

-- TODO: make this an Int, and priced in cents
type Price
  = Number

setControlledPrice :: TestContext -> Element -> Effect Unit
setControlledPrice testContext element = do
  className <- Element.className element
  let
    mTestMap = mElementSku >>= (\sku -> Map.lookup sku testContext.skuTestMap)
    mElementSku = getIdFromPriceElement element className
  liftEffect
    $ case mTestMap, Config.dryRunMode of
        (Just testMap), DryRun -> do
          formattedPrice <- Intl.formatPrice testMap.swapPrice
          Element.setAttribute "data-ssdr__price" formattedPrice element
        (Just testMap), Live -> do
          formattedPrice <- Intl.formatPrice testMap.swapPrice
          setTextContent formattedPrice (Element.toNode element)
        Nothing, _ -> pure unit

setControlledPriceM :: forall m. MonadEffect m => MonadAsk TestContext m => Element -> m Unit
setControlledPriceM element = do
  testContext <- Reader.ask
  liftEffect $ setControlledPrice testContext element

getIdFromPriceElement :: Element -> String -> Maybe Sku
getIdFromPriceElement el className =
  let classNames = String.split (String.Pattern " ") className
   in
  findSweetSpotTag classNames >>= getSkuFromTag <#> Sku
  where
  findSweetSpotTag :: Array String -> Maybe String
  findSweetSpotTag = Array.find (String.contains (String.Pattern Config.idClass))

  getSkuFromTag :: String -> Maybe String
  getSkuFromTag tag = Array.last $ String.split (String.Pattern "--") tag
