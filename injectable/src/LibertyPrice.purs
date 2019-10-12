module SweetSpot.LibertyPrice where

import Prelude
import Control.Monad.Reader (ask) as Reader
import Control.Monad.Reader (class MonadAsk)
import Data.Foldable (traverse_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestContext, VariantId(..))
import SweetSpot.Longvadon.Shared (onElementsMutation) as Shared
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.ParentNode (QuerySelector(..))

productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "#ProductSelect-product-template option"

setCheckout :: forall m. BrowserAction m => MonadEffect m => MonadAsk TestContext m => m Unit
setCheckout = do
  elements <- liftEffect $ SiteC.queryDocument productCheckoutOptionSelector
  traverse_ setCheckoutOption elements

setCheckoutOption :: forall m. BrowserAction m => MonadEffect m => MonadAsk TestContext m => Element -> m Unit
setCheckoutOption el = do
  { variantIdTestMap } <- Reader.ask
  mRawVariantId <- liftEffect $ SiteC.getAttribute "value" el
  let
    mVariantId = VariantId <$> mRawVariantId

    mSwapId = mVariantId >>= flip Map.lookup variantIdTestMap <#> _.swapId
  case mSwapId, dryRunMode of
    Nothing, _ -> pure unit
    (Just swapId), DryRun -> SiteC.setAttribute "data-ssdr__value" swapId el
    (Just swapId), Live -> SiteC.setAttribute "value" swapId el

observePrices :: forall m. MonadEffect m => MonadAsk TestContext m => BrowserAction m => m Unit
observePrices = do
  testContext <- Reader.ask
  priceElements <- SiteC.queryDocument SiteC.priceElementSelector
  Shared.onElementsMutation priceElements { characterData: true } $ traverse_ (SiteC.setControlledPrice testContext)
