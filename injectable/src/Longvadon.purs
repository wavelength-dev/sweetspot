module SweetSpot.Longvadon where

import Prelude
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Global (readFloat)
import SweetSpot.DOM (getOptionVariantId)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import Web.DOM (Element)
import Web.DOM.Document as Doc
import Web.DOM.Element as E
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window as Win

collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
collectCheckoutOptions variantIds = do
  htmlDoc <- window >>= Win.document
  let
    docNode = Doc.toParentNode <<< toDocument $ htmlDoc
  checkoutOptionNodes <- querySelectorAll (QuerySelector "[data-varid]") docNode
  nodesArray <- NL.toArray checkoutOptionNodes
  -- We expect these elements to be coercible to Element, so we ignore nodes which can not be converted to elements, as they shouldn't exist.
  let
    elements = A.catMaybes (map E.fromNode nodesArray)
  A.filterA getIsKnownVariantOption elements
  where
  getIsKnownVariantOption :: Element -> Effect Boolean
  getIsKnownVariantOption el = do
    dataVarid <- E.getAttribute "data-varid" el
    pure $ maybe false (\id -> A.elem (readFloat id) variantIds) dataVarid

swapCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
swapCheckoutVariantId buckets elements = traverse_ swapCheckoutIds elements
  where
  swapCheckoutIds :: Element -> Effect Unit
  swapCheckoutIds el = do
    mVariantId <- getOptionVariantId buckets "data-varid" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> E.setAttribute "ssdr__data-varid" variantId el
      (Just variantId), Live -> E.setAttribute "data-varid" variantId el
