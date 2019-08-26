module SweetSpot.Longvadon where

import Prelude

import Data.Array (catMaybes)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import SweetSpot.DOM (getOptionVariantId, nodesToElements, queryDocument)
import SweetSpot.Data.Api (TestMap)
import SweetSpot.Data.Config (DryRunMode(..), dryRunMode)
import Web.DOM (Element)
import Web.DOM.Element as El
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode (QuerySelector(..))

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
swapCheckoutVariantId :: NonEmptyArray TestMap -> Effect Unit
swapCheckoutVariantId testMaps = do
  nodeList <- queryDocument (QuerySelector "select.product-form__master-select option")
  nodes <- NL.toArray nodeList
  let
    elements = catMaybes $ map El.fromNode nodes
  for_ (elements :: Array Element) \el -> do
    mVariantId <- getOptionVariantId testMaps "value" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> El.setAttribute "data-ssdr__value" variantId el
      (Just variantId), Live -> El.setAttribute "value" variantId el


replaceTestVariantUrlOnCart :: Effect Unit
replaceTestVariantUrlOnCart = do
  nodeList <- queryDocument (QuerySelector "[href*=-ssv]")
  elements <- nodesToElements nodeList
  for_ elements \el -> do
    href <- El.getAttribute "href" el
    case href of
      (Just h) -> do
        let fixed = replace urlPattern "" h
        case dryRunMode of
          Live -> El.setAttribute "href" fixed el
          DryRun -> El.setAttribute "data-sshref" fixed el
      Nothing -> pure unit
  where
    urlPattern = unsafeRegex "^/collections/" ignoreCase
