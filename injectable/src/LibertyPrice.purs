module SweetSpot.LibertyPrice where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMap, getSwapId)
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities (getAttribute, queryDocument, setAttribute) as SiteC
import Web.DOM (Element)
import Web.DOM.ParentNode (QuerySelector(..))

productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "#ProductSelect-product-template option"

setCheckout :: forall m. DomAction m => Array TestMap -> m Unit
setCheckout testMaps = SiteC.queryDocument productCheckoutOptionSelector
  >>= traverse_ (setCheckoutOption testMaps)

setCheckoutOption :: forall m. DomAction m => Array TestMap -> Element -> m Unit
setCheckoutOption testMaps el = do
  mSwapId <- SiteC.getAttribute "value" el <#> (\mVariantId -> mVariantId >>= getSwapId testMaps)
  case mSwapId, dryRunMode of
    Nothing, _ -> pure unit
    (Just swapId), DryRun -> SiteC.setAttribute "data-ssdr__value" swapId el
    (Just swapId), Live -> SiteC.setAttribute "value" swapId el
