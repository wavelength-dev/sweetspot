module SweetSpot.Longvadon where

import Prelude

import Data.Array (find) as Array
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import SweetSpot.Data.Api (TestMap)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.ParentNode (QuerySelector(..))

-- Longvadon needs to be better inspected as to how it decides what to add to cart
-- collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
-- collectCheckoutOptions variantIds = do
--   htmlDoc <- window >>= Win.document
--   let
--     docNode = Doc.toParentNode <<< toDocument $ htmlDoc
--   checkoutOptionNodes <- querySelectorAll (QuerySelector "[data-varid]") docNode
--   nodesArray <- NL.toArray checkoutOptionNodes
--   -- We expect these elements to be coercible to Element, so we ignore nodes which can not be converted to elements, as they shouldn't exist.
--   let
--     elements = A.catMaybes (map E.fromNode nodesArray)
--   A.filterA getIsKnownVariantOption elements
--   where
--   getIsKnownVariantOption :: Element -> Effect Boolean
--   getIsKnownVariantOption el = do
--     dataVarid <- E.getAttribute "data-varid" el
--     pure $ maybe false (\id -> A.elem (readFloat id) variantIds) dataVarid
-- swapCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
-- swapCheckoutVariantId buckets elements = traverse_ swapCheckoutIds elements
--   where
--   swapCheckoutIds :: Element -> Effect Unit
--   swapCheckoutIds el = do
--     mVariantId <- getOptionVariantId buckets "data-varid" el
--     case mVariantId, dryRunMode of
--       Nothing, _ -> pure unit
--       (Just variantId), DryRun -> E.setAttribute "ssdr__data-varid" variantId el
--       (Just variantId), Live -> E.setAttribute "data-varid" variantId el
-- This is the way we collect checkout options on LibertyPrice that appeared not to work on Longvadon before.
-- Product Page Add to Cart Query Selector
-- Matches the elements that determine the product that is added to cart on the product page.
productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "select.product-form__master-select option"

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
slickCarouselOptionSelector :: QuerySelector
slickCarouselOptionSelector = QuerySelector "#color-slider input[value]"
isSoldOutElement :: Element -> Effect Boolean
isSoldOutElement el = SiteC.getAttribute "data-stock" el >>= maybe false ((==) "deny") >>> pure

setCheckout :: forall m. DomAction m => Array TestMap -> m Unit
setCheckout testMaps = do
  SiteC.queryDocument productCheckoutOptionSelector >>= traverse_ (SiteC.setCheckoutOption testMaps)
  SiteC.queryDocument slickCarouselOptionSelector >>= traverse_ (setSlickCarousel testMaps)

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
setSlickCarousel testMaps element = findMatchingTargetMap element >>= case _ of
        Nothing -> pure unit
        Just testMap -> SiteC.setAttribute "value" testMap.swapId element
  where
  findMatchingTargetMap el = do
    mVariantId <- SiteC.getAttribute "value" el
    pure
      $ case mVariantId of
          Nothing -> Nothing
          Just variantId -> Array.find (_.targetId >>> ((==) variantId)) testMaps

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
          DryRun -> SiteC.setAttribute "data-sshref" fixed el
      Nothing -> pure unit
  where
  urlPattern = unsafeRegex "^/collections/" ignoreCase
