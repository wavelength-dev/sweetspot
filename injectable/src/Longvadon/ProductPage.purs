module SweetSpot.Longvadon.ProductPage where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (noFlags) as RegexFlags
import Data.String.Regex.Unsafe (unsafeRegex) as RegexUnsafe
import Effect (Effect)
import SweetSpot.Data.Config (hiddenPriceId) as Config
import SweetSpot.Data.Domain (Sku(..), TestMap, VariantId)
import SweetSpot.Intl (formatPrice) as Intl
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import SweetSpot.Longvadon.Shared (onElementsMutation, setCheckoutOption) as Shared
import SweetSpot.SiteCapabilities (queryDocument, setControlledPrice) as SiteC
import Web.DOM (Element)
import Web.DOM.Element (getAttribute, toNode) as Element
import Web.DOM.MutationObserver as MutationObserver
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
setProductAddToCartButtonControlledPrice :: Map VariantId TestMap -> Effect Unit
setProductAddToCartButtonControlledPrice testMapsMap =
  SiteC.queryDocument productAddToCartButtonPriceSelector
    >>= traverse_ (SiteC.setControlledPrice testMapsMap)

observeProductAddToCartButton :: Map VariantId TestMap -> Effect Unit
observeProductAddToCartButton testMapsMap = do
  addToCartButtons <- SiteC.queryDocument productAddToCartButtonSelector
  mutationObserver <- MutationObserver.mutationObserver onMutation
  -- We expect there to be one
  for_ addToCartButtons \button ->
    MutationObserver.observe (Element.toNode button) { childList: true, subtree: true } mutationObserver
  where
  -- As the children of this element are replaced we can't monitor the actual price element for changes but instead monitor its parent. This means the element passed to the callback is a parent. We could traverse down using properties on this node, but instead elect to simply use another querySelectorAll that should now give us the price element that was just created.
  onMutation _ _ = do
    -- We expect there to be one
    priceElements <- SiteC.queryDocument productAddToCartButtonPriceSelector
    for_ priceElements \priceElement -> SiteC.setControlledPrice testMapsMap priceElement

observeProductSlickCarousel :: Map VariantId TestMap -> Effect Unit
observeProductSlickCarousel testMapsMap = do
  addToCartInputSelectors <- SiteC.queryDocument productSlickAddToCartInputSelector
  Shared.onElementsMutation addToCartInputSelectors { attributes: true } $ traverse_ setCheckoutOption'
  where
  setCheckoutOption' = Shared.setCheckoutOption testMapsMap

setProductVariantSelectorSources :: Map Sku TestMap -> Effect Unit
setProductVariantSelectorSources testMapsMap = do
  variantSelectorOptionElements <- SiteC.queryDocument productAddToCartOptionSelector
  traverse_ (setProductVariantSelectorSource testMapsMap) variantSelectorOptionElements

-- | Mutates the source of the product variant selector. Meaning:
-- | 1. subsequent variant selections set the price controlled price in the add-to-cart button
-- | 2. the correct variant is used for add-to-cart
-- <select name="id[]" class="product-form__master-select supports-no-js" data-master-select="">
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black42ClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="continue" data-sku="LVMens1Black42ClaspB" data-stock="775" value="30273507459115">42/44 / M / Black</option>
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black42LClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="continue" data-sku="LVMens1Black42LClaspB" data-stock="-9" value="30273507491883">42/44 / XL / Black</option>
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black38ClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="continue" data-sku="LVMens1Black38ClaspB" data-stock="58" value="30273507524651">38/40 / M / Black</option>
--   <option data-cartbtn="<span class=&quot;money&quot; style=&quot;text-decoration: line-through;opacity: 0.6;&quot;></span> <span class=&quot;money sweetspot__price--hidden sweetspot__price_id--LVMens1Black38LClaspB&quot;>$79</span><span> | ADD TO CART</span>" data-inplc="deny" data-sku="LVMens1Black38LClaspB" data-stock="-100" value="30273507557419">38/40 / XL / Black</option>
-- </select>
setProductVariantSelectorSource :: Map Sku TestMap -> Element -> Effect Unit
setProductVariantSelectorSource testMapsMap' variantOptionElement = do
  eRawHtml <- getRawHtml
  eSwapPrice <- getSwapPrice
  case eRawHtml, eSwapPrice of
    Left err, _ -> Log.log Error err
    _, Left err -> Log.log Error err
    Right rawHtml, Right swapPrice -> do
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

  getRawHtml :: Effect (Either String String)
  getRawHtml =
    Element.getAttribute "data-cartbtn" variantOptionElement
      >>= case _ of
          Nothing -> pure $ Left "Variant source element had empty data-cartbtn attribute!"
          Just rawHtml -> pure $ Right rawHtml

  getSwapPrice :: Effect (Either String Number)
  getSwapPrice = do
    mRawSku <- Element.getAttribute "data-sku" variantOptionElement
    let
      mSku = Sku <$> mRawSku
    case mSku of
      Nothing -> pure $ Left "No variant id on variant selector option!"
      Just sku -> case Map.lookup sku testMapsMap' of
        Nothing -> pure $ Left "Unknown variant in variant selector option!"
        Just testMap -> pure $ Right testMap.swapPrice

setVariantSelectorToControlledPrice :: Map VariantId TestMap -> Element -> Effect Unit
setVariantSelectorToControlledPrice testMapsMap variantOptionElement = SiteC.setControlledPrice testMapsMap variantOptionElement
