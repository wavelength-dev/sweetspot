module SweetSpot.Longvadon.ProductPage where

import Prelude
import Control.Monad.Reader (ask) as Reader
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (noFlags) as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex) as RegexUnsafe
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import SweetSpot.Data.Config (hiddenPriceId) as Config
import SweetSpot.Data.Domain (Sku(..), TestContext)
import SweetSpot.Intl (formatPrice) as Intl
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import SweetSpot.Longvadon.Shared (onElementsMutation, setCheckoutOption) as Shared
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities (getAttribute, queryDocument, setControlledPrice, setControlledPriceM) as SiteC
import Web.DOM (Element)
import Web.DOM.Element (getAttribute) as Element
import Web.DOM.ParentNode (QuerySelector(..))

productAddToCartOptionSelector :: QuerySelector
productAddToCartOptionSelector = QuerySelector "select.product-form__master-select option"

-- Consider trying to improve this selector. It's pretty good but dives about five levels deep.
productSlickAddToCartInputSelector :: QuerySelector
productSlickAddToCartInputSelector = QuerySelector "#color-slider input[value]"

productAddToCartButtonSelector :: QuerySelector
productAddToCartButtonSelector = QuerySelector "form.product-form button.product__add-to-cart-button"

productAddToCartButtonPriceSelector :: QuerySelector
productAddToCartButtonPriceSelector = QuerySelector "form.product-form button.product__add-to-cart-button span[class*=sweetspot__price_id--]"

type RawHtml
  = String

foreign import setCachedButtonHtml :: Element -> RawHtml -> Effect Unit

-- Here we cannot observe mutations for Longvadon. They seem to run a script that freezes the page in Safari when used in combination with ours.
-- <button class="add_cart btn btn--to-secondary btn--full product__add-to-cart-button   show" data-cart-submit="" type="submit" name="add" aria-live="polite">
--     <span class="primary-text prdistxt" aria-hidden="false" data-cart-primary-submit-text=""><span class="money" style="text-decoration: line-through;opacity: 0.6;"></span> <span class="money sweetspot__price--hidden sweetspot__price_id--LVMens1Black42ClaspB">$79</span><span> | ADD TO CART</span></span>
--     <span class="secondary-text" aria-hidden="true" data-cart-secondary-submit-text="">View cart</span>
-- </button>
setProductAddToCartButtonControlledPrice :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
setProductAddToCartButtonControlledPrice = SiteC.queryDocument productAddToCartButtonPriceSelector >>= traverse_ SiteC.setControlledPriceM

observeProductAddToCartButton :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
observeProductAddToCartButton = do
  testContext <- Reader.ask
  addToCartButtons <- SiteC.queryDocument productAddToCartButtonSelector
  Shared.onElementsMutation addToCartButtons { childList: true, subtree: true }
    $
     -- As the children of this element are replaced we can't monitor the actual price element for changes but instead monitor its parent. This means the element passed to the callback is a parent. We could traverse down using properties on this node, but instead elect to simply use another querySelectorAll that should now give us the price element that was just created. -- We expect there to be one
      \_ -> SiteC.queryDocument productAddToCartButtonPriceSelector >>= traverse_ (SiteC.setControlledPrice testContext)

-- instance browserActionReaderT :: BrowserAction m => ReaderT c m where
--   getAttribute = getAttribute
observeProductSlickCarousel :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
observeProductSlickCarousel = do
  testContext <- Reader.ask
  addToCartInputSelectors <- SiteC.queryDocument productSlickAddToCartInputSelector
  -- Shared.onElementsMutation addToCartInputSelectors { attributes: true } $ traverse_ Shared.setCheckoutOption
  Shared.onElementsMutation addToCartInputSelectors { attributes: true } $ traverse_ (Shared.setCheckoutOption testContext)

-- where onMutation :: forall m. MonadEffect m => BrowserAction m => TestContext -> Array Element -> m Unit
--       onMutation testContext = traverse_ \element -> flip Reader.runReaderT testContext (Shared.setCheckoutOption element)
setProductVariantSelectorSources :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
setProductVariantSelectorSources = SiteC.queryDocument productAddToCartOptionSelector >>= traverse_ setProductVariantSelectorSource

-- | Mutates the source of the product variant selector. Meaning:
-- | 1. subsequent variant selections set the price controlled price in the add-to-cart button
-- | 2. the correct variant is used for add-to-cart
-- <select name="id[]" class="product-form__master-select supports-no-js" data-master-select="">
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black42ClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="continue" data-sku="LVMens1Black42ClaspB" data-stock="775" value="30273507459115">42/44 / M / Black</option>
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black42LClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="continue" data-sku="LVMens1Black42LClaspB" data-stock="-9" value="30273507491883">42/44 / XL / Black</option>
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black38ClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="continue" data-sku="LVMens1Black38ClaspB" data-stock="58" value="30273507524651">38/40 / M / Black</option>
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black38LClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="deny" data-sku="LVMens1Black38LClaspB" data-stock="-100" value="30273507557419">38/40 / XL / Black</option>
-- </select>
setProductVariantSelectorSource :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => Element -> m Unit
setProductVariantSelectorSource variantOptionElement = do
  eRawHtml <- getRawHtml
  eSwapPrice <- getSwapPrice
  liftEffect case eRawHtml, eSwapPrice of
    Left err, _ -> Log.log Error err
    _, Left err -> Log.log Error err
    Right rawHtml, Right swapPrice ->
      liftEffect do
        localSwapPrice <- Intl.formatPrice swapPrice
        -- Set the price, then remove the obscuring CSS class.
        rawHtml
          # Regex.replace priceRegex (">" <> localSwapPrice <> "<")
          >>> Regex.replace hiddenPriceClassRegex ""
          >>> setCachedButtonHtml variantOptionElement
  where
  priceRegex :: Regex
  priceRegex = RegexUnsafe.unsafeRegex """>\$.*?<""" RegexFlags.noFlags

  hiddenPriceClassRegex :: Regex
  hiddenPriceClassRegex = RegexUnsafe.unsafeRegex Config.hiddenPriceId RegexFlags.noFlags

  getRawHtml :: m (Either String String)
  getRawHtml =
    SiteC.getAttribute "data-cartbtn" variantOptionElement
      >>= case _ of
          Nothing -> Left "Variant source element had empty data-cartbtn attribute!"
          Just rawHtml -> Right rawHtml
      >>> pure

  getSwapPrice :: m (Either String Number)
  getSwapPrice = do
    { skuTestMap } <- Reader.ask
    mRawSku <- liftEffect $ Element.getAttribute "data-sku" variantOptionElement
    let
      mSku = Sku <$> mRawSku
    pure
      $ case mSku of
          Nothing -> Left "No variant id on variant selector option!"
          Just sku -> case Map.lookup sku skuTestMap of
            Nothing -> Left "Unknown variant in variant selector option!"
            Just testMap -> Right testMap.swapPrice
