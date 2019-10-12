module SweetSpot.Longvadon
  ( attachObservers
  , convertSsvCollectionUrls
  , setCheckout
  , setProductAddToCartButtonControlledPrice
  , setProductVariantSelectorSources
  ) where

import Prelude

import Control.Monad.Reader (ask) as Reader
import Control.Monad.Reader (class MonadAsk)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect.Class (class MonadEffect)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestContext)
import SweetSpot.Longvadon.CartPage (cartSlickCarouselAddToCartButtonSelector, cartSlickCarouselOptionSelector, observeSlickButtons, setCheckoutSlickCheckout, setSlickCheckoutOption) as CartPage
import SweetSpot.Longvadon.ProductPage (observeProductSlickCarousel, productAddToCartOptionSelector, productSlickAddToCartInputSelector, setProductAddToCartButtonControlledPrice, setProductVariantSelectorSources) as ProductPage
import SweetSpot.Longvadon.Shared (onElementsMutation)
import SweetSpot.Longvadon.Shared (setCheckoutOptionM) as Shared
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities (getAttribute, priceElementSelector, queryDocument, setAttribute, setControlledPrice) as SiteC
import Web.DOM (Element)
import Web.DOM.ParentNode (QuerySelector(..))

-- Longvadon has four known price forms
-- collections page, every product in a collection lists a price
-- product page, variant selectiion, price and add to cart button
-- product page, add to add to cart slider
-- cart page, add to cart slider
-- Product Page Add to Cart Query Selector
-- Matches the elements that determine the product that is added to cart on the product page.
setCheckout :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
setCheckout = do
  -- Makes sure the correct variant is added to cart on product page.
  SiteC.queryDocument ProductPage.productAddToCartOptionSelector >>= traverse_ (Shared.setCheckoutOptionM)
  -- Makes sure the correct variant is co-added to cart with the slick carousel on product page.
  SiteC.queryDocument ProductPage.productSlickAddToCartInputSelector >>= traverse_ (Shared.setCheckoutOptionM)
  -- Makes sure the correct variant is offered to add to cart, when selecting a variant from the slick carousel on the cart page.
  SiteC.queryDocument CartPage.cartSlickCarouselOptionSelector >>= traverse_ (CartPage.setCheckoutSlickCheckout)
  -- Makes sure the correct variant is added to cart from the slick carousel on the cart page, without ever selecting a variant.
  SiteC.queryDocument CartPage.cartSlickCarouselAddToCartButtonSelector >>= traverse_ (CartPage.setSlickCheckoutOption)

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

observePrices :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
observePrices = do
  testContext <- Reader.ask
  elements <- SiteC.queryDocument SiteC.priceElementSelector
  onElementsMutation
    elements
    { characterData: true }
    $ traverse_ (SiteC.setControlledPrice testContext)

attachObservers :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
attachObservers =
  observePrices
    *> CartPage.observeSlickButtons
    *> ProductPage.observeProductSlickCarousel

setProductAddToCartButtonControlledPrice :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
setProductAddToCartButtonControlledPrice = ProductPage.setProductAddToCartButtonControlledPrice

setProductVariantSelectorSources :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
setProductVariantSelectorSources = ProductPage.setProductVariantSelectorSources
