module SweetSpot.LibertyPrice where

import Prelude

import Data.Foldable (traverse_)
import SweetSpot.Data.Api (TestMap)
import SweetSpot.SiteCapabilities (class DomAction)
import SweetSpot.SiteCapabilities (queryDocument, setCheckoutOption) as SiteC
import Web.DOM.ParentNode (QuerySelector(..))

productCheckoutOptionSelector :: QuerySelector
productCheckoutOptionSelector = QuerySelector "#ProductSelect-product-template option"

setCheckout :: forall m. DomAction m => Array TestMap -> m Unit
setCheckout testMaps = SiteC.queryDocument productCheckoutOptionSelector
  >>= traverse_ (SiteC.setCheckoutOption testMaps)
