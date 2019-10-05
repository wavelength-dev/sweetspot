module SweetSpot.LibertyPrice where

import Prelude
import Data.Foldable (for_, traverse_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMapsMap, VariantId(..))
import SweetSpot.Log (LogLevel(..))
import SweetSpot.Log (log) as Log
import SweetSpot.SiteCapabilities (class BrowserAction)
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.Element (fromNode, toNode) as Element
import Web.DOM.MutationObserver (MutationObserver)
import Web.DOM.MutationObserver (mutationObserver, observe) as MutationObserver
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord
import Web.DOM.ParentNode (QuerySelector(..))

productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "#ProductSelect-product-template option"

setCheckout :: forall m. BrowserAction m => TestMapsMap -> m Unit
setCheckout testMaps =
  SiteC.queryDocument productCheckoutOptionSelector
    >>= traverse_ (setCheckoutOption testMaps)

setCheckoutOption :: forall m. BrowserAction m => TestMapsMap -> Element -> m Unit
setCheckoutOption testMaps el = do
  mRawVariantId <- SiteC.getAttribute "value" el
  let
    mVariantId = VariantId <$> mRawVariantId
    mSwapId = mVariantId >>= flip Map.lookup testMaps <#> _.swapId
  case mSwapId, dryRunMode of
    Nothing, _ -> pure unit
    (Just swapId), DryRun -> SiteC.setAttribute "data-ssdr__value" swapId el
    (Just swapId), Live -> SiteC.setAttribute "value" swapId el

observePrices :: TestMapsMap -> Effect Unit
observePrices testMapsMap = do
  elements <- SiteC.queryDocument SiteC.priceElementSelector
  mutationObserver <- MutationObserver.mutationObserver onMutation
  for_ elements \el -> MutationObserver.observe (Element.toNode el) { characterData: true } mutationObserver
  where
  onMutation :: Array MutationRecord -> MutationObserver -> Effect Unit
  onMutation mutationRecords _ = do
    for_ mutationRecords \mutationRecord ->
      MutationRecord.target mutationRecord
        >>= \node -> case Element.fromNode node of
            Nothing -> Log.log Warn "Observed node was not an element."
            Just element -> SiteC.setControlledPrice testMapsMap element
