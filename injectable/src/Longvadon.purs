module SweetSpot.Longvadon where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMap, findMatchingTestMap)
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.ParentNode (QuerySelector(..))

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
cartSlickCarouselOptionSelector = QuerySelector "#buy input[value]"

isSoldOutElement :: Element -> Effect Boolean
isSoldOutElement el = SiteC.getAttribute "data-stock" el >>= maybe false ((==) "deny") >>> pure

setCheckout :: forall m. DomAction m => Array TestMap -> m Unit
setCheckout testMaps = do
  SiteC.queryDocument productCheckoutOptionSelector >>= traverse_ (setCheckoutOption testMaps)
  SiteC.queryDocument slickCarouselOptionSelector >>= traverse_ (setSlickCarousel testMaps)

-- Normal product page add-to-cart source. The id of the select is manipulated so that on form submit the right data gets sent by Shopify's add-to-cart script.
-- <select name="id[]" class="product-form__master-select supports-no-js" data-master-select="">
--   <option data-inplc="continue" data-sku="LVWomens1Pearl42ClaspB" data-stock="15" value="17028931551275">42/44 / M / Black</option>
--   <option data-inplc="deny" data-sku="LVWomens1Pearl42SClaspB" data-stock="-100" value="17028931584043">42/44 / XS / Black</option>
--   <option data-inplc="continue" data-sku="LVWomens1Pearl38ClaspB" data-stock="17" value="17028931649579">38/40 / M / Black</option>
--   <option data-inplc="deny" data-sku="LVWomens1Pearl38SClaspB" data-stock="-100" value="16408532844587">38/40 / XS / Black</option>
-- </select>
setCheckoutOption :: forall m. DomAction m => Array TestMap -> Element -> m Unit
setCheckoutOption testMaps el = do
  mVariantId <- SiteC.getAttribute "value" el
  let mTestMap = mVariantId >>= findMatchingTestMap testMaps
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    (Just testMap), DryRun -> SiteC.setAttribute "data-ssdr__value" testMap.swapId el
    (Just testMap), Live -> SiteC.setAttribute "value" testMap.swapId el

-- TODO: deal with price and add to cart in Slick carousel
-- button.product__add-to-cart-button
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
setSlickCarousel :: forall m. DomAction m => Array TestMap -> Element -> m Unit
setSlickCarousel testMaps element = do
  mVariantId <- SiteC.getAttribute "value" element
  let
    mTestMap = mVariantId >>= findMatchingTestMap testMaps
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun -> SiteC.setAttribute "data-ssdr__value" testMap.swapId element
    Just testMap, Live -> SiteC.setAttribute "value" testMap.swapId element

replaceTestVariantUrlOnCart :: forall m. DomAction m => m Unit
replaceTestVariantUrlOnCart = do
  elements <- SiteC.queryDocument (QuerySelector "[href*=-ssv]")
  for_ elements \el -> do
    href <- SiteC.getAttribute "href" el
    case href of
      (Just h) -> do
        let
          fixed = replace urlPattern "" h
        case dryRunMode of
          Live -> SiteC.setAttribute "href" fixed el
          DryRun -> SiteC.setAttribute "data-ssdr__href" fixed el
      Nothing -> pure unit
  where
  urlPattern = unsafeRegex "^/collections/" ignoreCase
