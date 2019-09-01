module SweetSpot.LibertyPrice where

import Prelude

import Data.Foldable (traverse_)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import SweetSpot.Data.Domain (TestMapsMap)
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities (getAttribute, queryDocument, setAttribute) as SiteC
import Web.DOM (Element)
import Web.DOM.ParentNode (QuerySelector(..))

productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "#ProductSelect-product-template option"

setCheckout :: forall m. DomAction m => TestMapsMap -> m Unit
setCheckout testMaps = SiteC.queryDocument productCheckoutOptionSelector
  >>= traverse_ (setCheckoutOption testMaps)

setCheckoutOption :: forall m. DomAction m => TestMapsMap -> Element -> m Unit
setCheckoutOption testMaps el = do
  mVariantId <- SiteC.getAttribute "value" el
  let mSwapId = mVariantId >>= flip Map.lookup testMaps <#> _.swapId
  case mSwapId, dryRunMode of
    Nothing, _ -> pure unit
    (Just swapId), DryRun -> SiteC.setAttribute "data-ssdr__value" swapId el
    (Just swapId), Live -> SiteC.setAttribute "value" swapId el
