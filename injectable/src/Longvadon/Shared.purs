module SweetSpot.Longvadon.Shared where

import Prelude
import Control.Monad.Reader (ask) as Reader
import Control.Monad.Reader (class MonadAsk)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Prim.Row (class Union)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMap, VariantId(..), TestContext)
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities (getAttribute, observeForMutation, setAttribute) as SiteC
import Web.DOM (Element)
import Web.DOM.Element (fromNode, toNode) as Element
import Web.DOM.MutationObserver (MutationObserverInitFields)
import Web.DOM.MutationObserver (mutationObserver) as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord

data StockStatus
  = Deny
  | Other

readStock :: String -> StockStatus
readStock = case _ of
  "deny" -> Deny
  _ -> Other

onElementsMutation ::
  forall r rx m.
  Union r rx MutationObserverInitFields =>
  MonadEffect m =>
  BrowserAction m =>
  Array Element -> Record r -> (Array Element -> Effect Unit) -> m Unit
onElementsMutation elements options callback = do
  mutationObserver <- liftEffect $ MutationObserver.mutationObserver (\mrs _ -> mutationRecordsToElements mrs >>= callback)
  let
    observe = Element.toNode >>> \node -> SiteC.observeForMutation node options mutationObserver
  traverse_ observe elements
  where
  -- We discard the possibilty of some observed nodes not being elements as the only nodes we watch are price elements which are necessarily Html elements.
  mutationRecordsToElements :: Array MutationRecord -> Effect (Array Element)
  mutationRecordsToElements mutationRecords =
    traverse (MutationRecord.target >>> liftEffect >=> Element.fromNode >>> pure) mutationRecords
      >>= Array.catMaybes
      >>> pure

isSoldOutElement :: forall m. BrowserAction m => Element -> m Boolean
isSoldOutElement el = SiteC.getAttribute "data-pric" el >>= maybe false isPriceSoldOut >>> pure
  where
  isPriceSoldOut = String.toLower >>> String.contains (Pattern "sold out")

-- Normal product page add-to-cart source. The id of the select is manipulated so that on form submit the right data gets sent by Shopify's add-to-cart script.
-- <select name="id[]" class="product-form__master-select supports-no-js" data-master-select="">
--   <option data-inplc="continue" data-sku="LVWomens1Pearl42ClaspB" data-stock="15" value="17028931551275">42/44 / M / Black</option>
--   <option data-inplc="deny" data-sku="LVWomens1Pearl42SClaspB" data-stock="-100" value="17028931584043">42/44 / XS / Black</option>
--   <option data-inplc="continue" data-sku="LVWomens1Pearl38ClaspB" data-stock="17" value="17028931649579">38/40 / M / Black</option>
--   <option data-inplc="deny" data-sku="LVWomens1Pearl38SClaspB" data-stock="-100" value="16408532844587">38/40 / XS / Black</option>
-- </select>
-- Also used for slick checkout options
-- <input class="check--color" type="checkbox" name="id[]" value="20609191706667" tabindex="-1">
setCheckoutOption :: forall m. BrowserAction m => TestContext -> Element -> m Unit
setCheckoutOption testContext element = do
  mRawVariantId <- SiteC.getAttribute "value" element
  let
    mVariantId = mRawVariantId <#> VariantId

    mTestMap = mVariantId >>= getTestMap testContext.variantIdTestMap
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun -> SiteC.setAttribute "data-ssdr__value" testMap.swapId element
    Just testMap, Live -> SiteC.setAttribute "value" testMap.swapId element
  where
  getTestMap :: Map VariantId TestMap -> VariantId -> Maybe TestMap
  getTestMap testMaps variantId = Map.lookup variantId testMaps

setCheckoutOptionM :: forall m. BrowserAction m => MonadAsk TestContext m => Element -> m Unit
setCheckoutOptionM element = do
  testContext <- Reader.ask
  setCheckoutOption testContext element