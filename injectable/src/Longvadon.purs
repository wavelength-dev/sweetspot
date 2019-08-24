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
import Web.DOM.HTMLCollection (toArray) as HC
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window as Win

-- Longvadon needs to be better inspected as to how it decides what to add to cart
-- collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
-- collectCheckoutOptions variantIds = do
--   htmlDoc <- window >>= Win.document
--   let
--     docNode = Doc.toParentNode <<< toDocument $ htmlDoc
--   checkoutOptionNodes <- querySelectorAll (QuerySelector "[data-varid]") docNode
--   nodesArray <- NL.toArray checkoutOptionNodes
--   -- We expect these elements to be coercible to Element, so we ignore nodes which can not be converted to elements, as they shouldn't exist.
--   let
--     elements = A.catMaybes (map E.fromNode nodesArray)
--   A.filterA getIsKnownVariantOption elements
--   where
--   getIsKnownVariantOption :: Element -> Effect Boolean
--   getIsKnownVariantOption el = do
--     dataVarid <- E.getAttribute "data-varid" el
--     pure $ maybe false (\id -> A.elem (readFloat id) variantIds) dataVarid

-- swapCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
-- swapCheckoutVariantId buckets elements = traverse_ swapCheckoutIds elements
--   where
--   swapCheckoutIds :: Element -> Effect Unit
--   swapCheckoutIds el = do
--     mVariantId <- getOptionVariantId buckets "data-varid" el
--     case mVariantId, dryRunMode of
--       Nothing, _ -> pure unit
--       (Just variantId), DryRun -> E.setAttribute "ssdr__data-varid" variantId el
--       (Just variantId), Live -> E.setAttribute "data-varid" variantId el

-- This is the way we collect checkout options on LibertyPrice that appeared not to work on Longvadon before.
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

swapCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
swapCheckoutVariantId buckets = traverse_ swapCheckoutIds
  where
  swapCheckoutIds :: Element -> Effect Unit
  swapCheckoutIds el = do
    mVariantId <- getOptionVariantId buckets "value" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> E.setAttribute "ssdr__value" variantId el
      (Just variantId), Live -> E.setAttribute "value" variantId el
