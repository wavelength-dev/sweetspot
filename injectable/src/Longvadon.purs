module SweetSpot.Longvadon where

import Prelude
import Data.Foldable (for_, traverse_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String (stripPrefix) as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import SweetSpot.Api (postLogPayload) as Api
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMapsMap)
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.Element (fromNode, toNode, toParentNode) as Element
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DOM.ParentNode (querySelector) as ParentNode

-- Longvadon has four known price forms
-- collections page, every product in a collection lists a price
-- product page, variant selectiion, price and add to cart button
-- product page, add to add to cart slider
-- cart page, add to cart slider
-- Product Page Add to Cart Query Selector
-- Matches the elements that determine the product that is added to cart on the product page.
productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "select.product-form__master-select option"

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
slickCarouselOptionSelector :: QuerySelector
slickCarouselOptionSelector = QuerySelector "#color-slider input[value]"

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
cartSlickCarouselOptionSelector :: QuerySelector
cartSlickCarouselOptionSelector = QuerySelector "#buy option[value]"

cartSlickCarouselCheckoutButtonSelector :: QuerySelector
cartSlickCarouselCheckoutButtonSelector = QuerySelector "button.product__add-to-cart-button"

isSoldOutElement :: Element -> Effect Boolean
isSoldOutElement el = SiteC.getAttribute "data-stock" el >>= maybe false ((==) "deny") >>> pure

setCheckout :: forall m. DomAction m => TestMapsMap -> m Unit
setCheckout testMaps = do
  SiteC.queryDocument productCheckoutOptionSelector >>= traverse_ (setCheckoutOption testMaps)
  SiteC.queryDocument slickCarouselOptionSelector >>= traverse_ (setCheckoutOption testMaps)
  SiteC.queryDocument cartSlickCarouselOptionSelector >>= traverse_ (setCheckoutOption testMaps)
  SiteC.queryDocument cartSlickCarouselCheckoutButtonSelector >>= traverse_ (setSlickCheckoutOption testMaps)

-- Normal product page add-to-cart source. The id of the select is manipulated so that on form submit the right data gets sent by Shopify's add-to-cart script.
-- <select name="id[]" class="product-form__master-select supports-no-js" data-master-select="">
--   <option data-inplc="continue" data-sku="LVWomens1Pearl42ClaspB" data-stock="15" value="17028931551275">42/44 / M / Black</option>
--   <option data-inplc="deny" data-sku="LVWomens1Pearl42SClaspB" data-stock="-100" value="17028931584043">42/44 / XS / Black</option>
--   <option data-inplc="continue" data-sku="LVWomens1Pearl38ClaspB" data-stock="17" value="17028931649579">38/40 / M / Black</option>
--   <option data-inplc="deny" data-sku="LVWomens1Pearl38SClaspB" data-stock="-100" value="16408532844587">38/40 / XS / Black</option>
-- </select>
setCheckoutOption :: forall m. DomAction m => TestMapsMap -> Element -> m Unit
setCheckoutOption testMaps el = do
  mVariantId <- SiteC.getAttribute "value" el
  let
    mTestMap = mVariantId >>= flip Map.lookup testMaps
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun -> SiteC.setAttribute "data-ssdr__value" testMap.swapId el
    Just testMap, Live -> SiteC.setAttribute "value" testMap.swapId el

-- Deal with price and add to cart in Slick carousel.
-- button.product__add-to-cart-button
setSlickCheckoutOption :: forall m. DomAction m => TestMapsMap -> Element -> m Unit
setSlickCheckoutOption testMaps el = do
  mCurrentVariantId <- SiteC.getAttribute "data-vrnt" el
  let
    mTestMap = mCurrentVariantId >>= flip Map.lookup testMaps
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun -> SiteC.setAttribute "data-ssdr__vrnt" testMap.swapId el
    Just testMap, Live -> SiteC.setAttribute "data-vrnt" testMap.swapId el

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
-- takes a collections URL of shape: /collections/all/products/womens-pearl-gray-w-black-details?variant=15404845662251 and removes the /collections/all bit so it becomes a product URL.
convertSsvCollectionUrls :: forall m. DomAction m => m Unit
convertSsvCollectionUrls = SiteC.queryDocument (QuerySelector "[href*=-ssv]") >>= traverse_ updateLink
  where
  updateLink :: Element -> m Unit
  updateLink el = do
    mHref <- SiteC.getAttribute "href" el
    let
      mProductUrl = mHref
        >>= String.stripPrefix (Pattern "/collections/")
        >>= String.stripSuffix (Pattern "-ssv")
    case mProductUrl, dryRunMode of
      Nothing, _ -> pure unit
      Just productUrl, Live -> SiteC.setAttribute "href" productUrl el
      Just productUrl, DryRun -> SiteC.setAttribute "data-ssdr__href" productUrl el

-- The traversal here is a bit risky. We watch for price elements being touched, this is taken as our que that the cart page slick carousel add to cart button has also been updated with a variant that possibly has a test price variant associated. We therefore find the button using an assumption heavy DOM traversal and reset the button to its correct state.
resetSlickAddToCartButton :: TestMapsMap -> Element -> Effect Unit
resetSlickAddToCartButton testMapsMap mutatedPriceElement = do
  let
    parentNode = Element.toParentNode mutatedPriceElement
  mButton <- ParentNode.querySelector (QuerySelector "button.product__add-to-cart-button") parentNode
  maybe (pure unit) (setSlickCheckoutOption testMapsMap) mButton

observePrices :: TestMapsMap -> Effect Unit
observePrices testMapsMap = do
  elements <- SiteC.queryDocument SiteC.priceElementSelector
  mutationObserver <- MutationObserver.mutationObserver onMutation
  for_ elements \el -> MutationObserver.observe (Element.toNode el) { characterData: true } mutationObserver
  where
  onMutation :: Array MutationRecord → MutationObserver → Effect Unit
  onMutation mutationRecords _ = do
    for_ mutationRecords \mutationRecord ->
      MutationRecord.target mutationRecord
        >>= \node -> case Element.fromNode node of
            Nothing -> launchAff_ $ Api.postLogPayload "WARN: observed node was not an element"
            Just element -> SiteC.applyPriceVariation testMapsMap element

observeButtons :: TestMapsMap -> Effect Unit
observeButtons testMapsMap = do
  elements <- SiteC.queryDocument cartSlickCarouselCheckoutButtonSelector
  mutationObserver <- MutationObserver.mutationObserver onMutation
  for_ elements \el -> MutationObserver.observe (Element.toNode el) { attributes: true, attributeFilter: [ "data-vrnt" ] } mutationObserver
  where
  onMutation :: Array MutationRecord → MutationObserver → Effect Unit
  onMutation mutationRecords _ = do
    for_ mutationRecords \mutationRecord ->
      MutationRecord.target mutationRecord
        >>= \node -> case Element.fromNode node of
            Nothing -> launchAff_ $ Api.postLogPayload "WARN: observed node was not an element"
            Just element -> setCheckoutOption testMapsMap element

attachObservers :: TestMapsMap -> Effect Unit
attachObservers testMapsMap = observePrices testMapsMap *> observeButtons testMapsMap
