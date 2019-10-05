module SweetSpot.Longvadon.CartPage where

import Prelude
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (VariantId(..), TestMap)
import SweetSpot.Intl (formatPrice) as Intl
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import SweetSpot.Longvadon.Shared (StockStatus(..))
import SweetSpot.Longvadon.Shared (readStock, setCheckoutOption) as Shared
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities (getAttribute, queryDocument, setAttribute) as SiteC
import Web.DOM (Element)
import Web.DOM.Element (fromNode, toNode) as Element
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord
import Web.DOM.ParentNode (QuerySelector(..))

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
cartSlickCarouselOptionSelector :: QuerySelector
cartSlickCarouselOptionSelector = QuerySelector "#buy option[value]"

cartSlickCarouselAddToCartButtonSelector :: QuerySelector
cartSlickCarouselAddToCartButtonSelector = QuerySelector "button.product__add-to-cart-button"

-- Slick silder has a hidden select which is used as the source from which to update the price and add to cart button
-- We should check 'data-stock', if 'deny', leave the data-pric as it is, otherwise swap in our price.
-- Use the value attribute in combination with our buckets or testmaps
-- On first run we still need to trigger the logic that updates the button or update the button ourselves.
-- <div class="hidden">
--   <select class="slider--col" tabindex="0">
--     <option value="14346686332971" data-pric=" $79.00  " data-stock="continue">42/44 / M / Black</option>
--     <option value="16408520720427" data-pric=" Sold out" data-stock="continue">42/44 / XL / Black</option>
--     <option value="14346686365739" data-pric=" $79.00  " data-stock="continue">38/40 / M / Black</option>
--     <option value="16408520753195" data-pric=" Sold out" data-stock="deny">38/40 / XL / Black</option>
--   </select>
-- </div>
setCheckoutSlickCheckout :: Map VariantId TestMap -> Element -> Effect Unit
setCheckoutSlickCheckout testMaps el = do
  mRawVariantId <- SiteC.getAttribute "value" el
  mRawStockStatus <- SiteC.getAttribute "data-stock" el
  let
    mVariantId = mRawVariantId <#> VariantId

    mTestMap = mVariantId >>= flip Map.lookup testMaps

    mStockStatus = Shared.readStock <$> mRawStockStatus
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun ->
      SiteC.setAttribute "data-ssdr__value" testMap.swapId el
        *> case mStockStatus of
            Nothing -> do
              price <- Intl.formatPrice testMap.swapPrice
              SiteC.setAttribute "data-ssdr__pric" price el
            Just stockStatus -> do
              price <- getPrice testMap stockStatus
              SiteC.setAttribute "data-ssdr__pric" price el
    Just testMap, Live ->
      do
        SiteC.setAttribute "value" testMap.swapId el
        *> case mStockStatus of
            Nothing -> do
              price <- Intl.formatPrice testMap.swapPrice
              SiteC.setAttribute "data-pric" price el
            Just stockStatus -> do
              price <- getPrice testMap stockStatus
              SiteC.setAttribute "data-pric" price el
  where
  getPrice testMap = case _ of
    Deny -> pure "Sold out"
    Other -> Intl.formatPrice testMap.swapPrice

-- Deal with price and add to cart in Slick carousel on the cart page.
-- button.product__add-to-cart-button
-- <button class="btn product__add-to-cart-button my-additional-btn" data-cart-submit="" type="button" name="add" data-vrnt="20609192362027" tabindex="0">
--  ADD TO CART
-- </button>
setSlickCheckoutOption :: forall m. BrowserAction m => Map VariantId TestMap -> Element -> m Unit
setSlickCheckoutOption testMaps el = do
  mCurrentRawVariantId <- SiteC.getAttribute "data-vrnt" el
  let
    mCurrentVariantId = mCurrentRawVariantId <#> VariantId

    mTestMap = mCurrentVariantId >>= flip Map.lookup testMaps
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun -> SiteC.setAttribute "data-ssdr__vrnt" testMap.swapId el
    Just testMap, Live -> SiteC.setAttribute "data-vrnt" testMap.swapId el

observeSlickButtons :: Map VariantId TestMap -> Effect Unit
observeSlickButtons testMapsMap = do
  elements <- SiteC.queryDocument cartSlickCarouselAddToCartButtonSelector
  mutationObserver <- MutationObserver.mutationObserver onMutation
  for_ elements \el -> MutationObserver.observe (Element.toNode el) { attributes: true, attributeFilter: [ "data-vrnt" ] } mutationObserver
  where
  onMutation :: Array MutationRecord → MutationObserver → Effect Unit
  onMutation mutationRecords _ = do
    for_ mutationRecords \mutationRecord ->
      MutationRecord.target mutationRecord
        >>= \node -> case Element.fromNode node of
            Nothing -> Log.log Error "Observed node was not an element."
            Just element -> Shared.setCheckoutOption testMapsMap element
