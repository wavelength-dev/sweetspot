module SweetSpot.Longvadon
  ( attachObservers
  , convertSsvCollectionUrls
  , setCheckout
  , setProductAddToCartButtonControlledPrice
  , setProductVariantSelectorSources
  ) where

import Prelude
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (Sku, VariantId, TestMap)
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import SweetSpot.Longvadon.CartPage (cartSlickCarouselAddToCartButtonSelector, cartSlickCarouselOptionSelector, observeSlickButtons, setCheckoutSlickCheckout, setSlickCheckoutOption) as CartPage
import SweetSpot.Longvadon.ProductPage (observeProductSlickCarousel, productAddToCartOptionSelector, productSlickAddToCartInputSelector, setProductAddToCartButtonControlledPrice, setProductVariantSelectorSources) as ProductPage
import SweetSpot.Longvadon.Shared (setCheckoutOption) as Shared
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities (getAttribute, priceElementSelector, queryDocument, setAttribute, setControlledPrice) as SiteC
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
setCheckout :: Map VariantId TestMap -> Effect Unit
setCheckout testMaps = do
  -- Makes sure the correct variant is added to cart on product page.
  SiteC.queryDocument ProductPage.productAddToCartOptionSelector >>= traverse_ (Shared.setCheckoutOption testMaps)
  -- Makes sure the correct variant is co-added to cart with the slick carousel on product page.
  SiteC.queryDocument ProductPage.productSlickAddToCartInputSelector >>= traverse_ (Shared.setCheckoutOption testMaps)
  -- Makes sure the correct variant is offered to add to cart, when selecting a variant from the slick carousel on the cart page.
  SiteC.queryDocument CartPage.cartSlickCarouselOptionSelector >>= traverse_ (CartPage.setCheckoutSlickCheckout testMaps)
  -- Makes sure the correct variant is added to cart from the slick carousel on the cart page, without ever selecting a variant.
  SiteC.queryDocument CartPage.cartSlickCarouselAddToCartButtonSelector >>= traverse_ (CartPage.setSlickCheckoutOption testMaps)

-- takes a collections URL of shape: /collections/all/products/womens-pearl-gray-w-black-details?variant=15404845662251 and removes the /collections/all bit so it becomes a product URL. Also removes the `-ssv` from the slug so that link takes you to original product.
convertSsvCollectionUrls :: forall m. BrowserAction m => m Unit
convertSsvCollectionUrls = SiteC.queryDocument (QuerySelector "[href*=-ssv]") >>= traverse_ updateLink
  where
  updateLink :: Element -> m Unit
  updateLink el = do
    mHref <- SiteC.getAttribute "href" el
    let
      mProductUrl =
        mHref
          >>= String.split (Pattern "?")
          >>> Array.head
          >>= String.stripPrefix (Pattern "/collections/")
          >>= String.stripSuffix (Pattern "-ssv")
    case mProductUrl, dryRunMode of
      Nothing, _ -> pure unit
      Just productUrl, DryRun -> SiteC.setAttribute "data-ssdr__href" productUrl el
      Just productUrl, Live -> SiteC.setAttribute "href" productUrl el

observePrices :: Map VariantId TestMap -> Effect Unit
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
            Nothing -> Log.log Error "Observed node was not an element."
            Just element -> SiteC.setControlledPrice testMapsMap element

attachObservers :: Map VariantId TestMap -> Effect Unit
attachObservers testMapsMap =
  observePrices testMapsMap
    *> CartPage.observeSlickButtons testMapsMap
    *> ProductPage.observeProductSlickCarousel testMapsMap

setProductAddToCartButtonControlledPrice :: Map VariantId TestMap -> Effect Unit
setProductAddToCartButtonControlledPrice = ProductPage.setProductAddToCartButtonControlledPrice

setProductVariantSelectorSources :: Map Sku TestMap -> Effect Unit
setProductVariantSelectorSources = ProductPage.setProductVariantSelectorSources
