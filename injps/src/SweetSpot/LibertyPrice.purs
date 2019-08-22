module SweetSpot.LibertyPrice where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import SweetSpot.DOM (getOptionVariantId)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Constant (DryRunMode(..), dryRunMode)
import Web.DOM (Element)
import Web.DOM.Element (setAttribute) as E

swapLibertyPriceCheckoutVariantId ::
  NonEmptyArray UserBucket ->
  Array Element ->
  Effect Unit
swapLibertyPriceCheckoutVariantId buckets = traverse_ swapCheckoutIds
  where
  swapCheckoutIds :: Element -> Effect Unit
  swapCheckoutIds el = do
    mVariantId <- getOptionVariantId buckets "value" el
    case mVariantId, dryRunMode of
      Nothing, _ -> pure unit
      (Just variantId), DryRun -> E.setAttribute "ssdr__value" variantId el
      (Just variantId), Live -> E.setAttribute "value" variantId el
