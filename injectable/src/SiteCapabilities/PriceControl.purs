module SweetSpot.SiteCapabilities.PriceControl (setControlledPrice) where

import Prelude

import Control.Monad.Reader (ask) as Reader
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array as Array
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import SweetSpot.Data.Config (DryRunMode(..))
import SweetSpot.Data.Config (dryRunMode, idClass) as Config
import SweetSpot.Data.Domain (Sku(..), TestMap)
import SweetSpot.Intl (formatPrice) as Intl
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node (setTextContent)

-- TODO: make this an Int, and priced in cents
type Price
  = Number

setControlledPrice :: forall m. MonadEffect m => MonadAsk { testMaps :: Map Sku TestMap } m => Element -> m Unit
setControlledPrice el = do
  config <- Reader.ask
  mElementSku <- liftEffect $ getIdFromPriceElement el
  let
    mTestMap = mElementSku >>= (\sku -> Map.lookup sku config.testMaps)
  case mTestMap, Config.dryRunMode of
    (Just testMap), DryRun -> do
      formattedPrice <- Intl.formatPrice testMap.swapPrice
      Element.setAttribute "data-ssdr__price" formattedPrice el
    (Just testMap), Live -> do
      formattedPrice <- Intl.formatPrice testMap.swapPrice
      setTextContent formattedPrice (Element.toNode el)
    Nothing, _ -> pure unit

getIdFromPriceElement :: Element -> Effect (Maybe Sku)
getIdFromPriceElement el = do
  classNames <- (String.split $ String.Pattern " ") <$> Element.className el
  pure $ findSweetSpotTag classNames >>= getSkuFromTag <#> Sku
  where
  findSweetSpotTag :: Array String -> Maybe String
  findSweetSpotTag = Array.find (String.contains (String.Pattern Config.idClass))

  getSkuFromTag :: String -> Maybe String
  getSkuFromTag tag = Array.last $ String.split (String.Pattern "--") tag
