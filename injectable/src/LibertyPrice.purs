module SweetSpot.LibertyPrice where

import Prelude
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Global (readFloat)
import SweetSpot.DOM (getOptionVariantId)
import SweetSpot.Data.Api (TestMap)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import Web.DOM (Element)
import Web.DOM.Document as Doc
import Web.DOM.Element as E
import Web.DOM.HTMLCollection as HC
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window as Win

collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
collectCheckoutOptions variantIds = do
  doc <- window >>= Win.document
  elements <- Doc.getElementsByTagName "option" (toDocument doc) >>= HC.toArray
  -- return any element with a value attribute value equal to one of variantIds
  A.filterA getIsKnownVariantOption elements
  where
  getIsKnownVariantOption :: Element -> Effect Boolean
  getIsKnownVariantOption el = do
    optionId <- E.getAttribute "value" el
    pure $ maybe false (\id -> A.elem (readFloat id) variantIds) optionId

swapCheckoutVariantId :: NonEmptyArray TestMap -> Array Element -> Effect Unit
swapCheckoutVariantId testMaps = traverse_ swapCheckoutIds
  where
  swapCheckoutIds :: Element -> Effect Unit
  swapCheckoutIds el = do
    mVariantId <- getOptionVariantId testMaps "value" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> E.setAttribute "ssdr__value" variantId el
      (Just variantId), Live -> E.setAttribute "value" variantId el
