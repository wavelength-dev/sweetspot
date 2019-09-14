module SweetSpot.Longvadon where

import Prelude
import Data.Array as A
import Data.Foldable (for_, traverse_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import SweetSpot.Api (postLogPayload) as Api
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMapsMap)
import SweetSpot.Intl (formatPrice) as Intl
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.Element (fromNode, toNode) as Element
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord
import Web.DOM.ParentNode (QuerySelector(..))

-- Longvadon has four known price forms
-- collections page, every product in a collection lists a price
-- product page, variant selectiion, price and add to cart button
-- product page, add to add to cart slider
-- cart page, add to cart slider
-- Product Page Add to Cart Query Selector
-- Matches the elements that determine the product that is added to cart on the product page.
productAddToCartOptionSelector :: QuerySelector
productAddToCartOptionSelector = QuerySelector "select.product-form__master-select option"

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
productSlickAddToCartOptionSelector :: QuerySelector
productSlickAddToCartOptionSelector = QuerySelector "#color-slider input[value]"

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
cartSlickCarouselOptionSelector :: QuerySelector
cartSlickCarouselOptionSelector = QuerySelector "#buy option[value]"

cartSlickCarouselAddToCartButtonSelector :: QuerySelector
cartSlickCarouselAddToCartButtonSelector = QuerySelector "button.product__add-to-cart-button"

isSoldOutElement :: Element -> Effect Boolean
isSoldOutElement el = SiteC.getAttribute "data-stock" el >>= maybe false ((==) "deny") >>> pure

setCheckout :: TestMapsMap -> Effect Unit
setCheckout testMaps = do
  -- Makes sure the correct variant is added to cart on product page.
  SiteC.queryDocument productAddToCartOptionSelector >>= traverse_ (setCheckoutOption testMaps)
  -- Makes sure the correct variant is co-added to cart with the slick carousel on product page.
  SiteC.queryDocument productSlickAddToCartOptionSelector >>= traverse_ (setCheckoutOption testMaps)
  -- Makes sure the correct variant is offered to add to cart, when selecting a variant from the slick carousel on the cart page.
  SiteC.queryDocument cartSlickCarouselOptionSelector >>= traverse_ (setCheckoutSlickCheckout testMaps)
  -- Makes sure the correct variant is added to cart from the slick carousel on the cart page, without ever selecting a variant.
  SiteC.queryDocument cartSlickCarouselAddToCartButtonSelector >>= traverse_ (setSlickCheckoutOption testMaps)

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

data StockStatus
  = Deny
  | Other

readStock :: String -> StockStatus
readStock = case _ of
  "deny" -> Deny
  _ -> Other

setCheckoutSlickCheckout :: TestMapsMap -> Element -> Effect Unit
setCheckoutSlickCheckout testMaps el = do
  mVariantId <- SiteC.getAttribute "value" el
  mRawStockStatus <- SiteC.getAttribute "data-stock" el
  let
    mTestMap = mVariantId >>= flip Map.lookup testMaps

    mStockStatus = readStock <$> mRawStockStatus
  case mTestMap, dryRunMode of
    Nothing, _ -> pure unit
    Just testMap, DryRun ->
      SiteC.setAttribute "data-ssdr__value" testMap.swapId el
        *> case mStockStatus of
            Nothing -> SiteC.setAttribute "data-ssdr__pric" (show testMap.swapPrice) el
            Just stockStatus -> getPrice testMap stockStatus >>= \price -> SiteC.setAttribute "data-ssdr__pric" price el
    Just testMap, Live ->
      do
        SiteC.setAttribute "value" testMap.swapId el
        *> case mStockStatus of
            Nothing -> SiteC.setAttribute "data-pric" (show testMap.swapPrice) el
            Just stockStatus -> getPrice testMap stockStatus >>= \price -> SiteC.setAttribute "data-pric" price el
  where
  getPrice testMap = case _ of
    Deny -> pure "Sold out"
    Other -> Intl.formatPrice testMap.swapPrice

-- Deal with price and add to cart in Slick carousel.
-- button.product__add-to-cart-button
-- <button class="btn product__add-to-cart-button my-additional-btn" data-cart-submit="" type="button" name="add" data-vrnt="20609192362027" tabindex="0">
--  ADD TO CART
-- </button>
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
-- takes a collections URL of shape: /collections/all/products/womens-pearl-gray-w-black-details?variant=15404845662251 and removes the /collections/all bit so it becomes a product URL. Also removes the `-ssv` from the slug so that link takes you to original product.
convertSsvCollectionUrls :: forall m. DomAction m => m Unit
convertSsvCollectionUrls = SiteC.queryDocument (QuerySelector "[href*=-ssv]") >>= traverse_ updateLink
  where
  updateLink :: Element -> m Unit
  updateLink el = do
    mHref <- SiteC.getAttribute "href" el
    let
      mProductUrl =
        mHref
          >>= String.split (Pattern "?")
          >>> A.head
          >>= String.stripPrefix (Pattern "/collections/")
          >>= String.stripSuffix (Pattern "-ssv")
    case mProductUrl, dryRunMode of
      Nothing, _ -> pure unit
      Just productUrl, DryRun -> SiteC.setAttribute "data-ssdr__href" productUrl el
      Just productUrl, Live -> SiteC.setAttribute "href" productUrl el

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

observeSlickButtons :: TestMapsMap -> Effect Unit
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
            Nothing -> launchAff_ $ Api.postLogPayload "WARN: observed node was not an element"
            Just element -> setCheckoutOption testMapsMap element

attachObservers :: TestMapsMap -> Effect Unit
attachObservers testMapsMap = observePrices testMapsMap *> observeSlickButtons testMapsMap
