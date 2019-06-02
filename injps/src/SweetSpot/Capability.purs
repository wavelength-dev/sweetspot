module SweetSpot.Capability where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import SweetSpot.Data.Api (UserBucket)

class Monad m <= AppCapability m where
  ensureDeps :: m Unit
  getUserId :: m (Maybe String)
  setUserId :: UserBucket -> m Unit
  getUserBuckets :: Maybe String -> Maybe String -> m (NonEmptyArray UserBucket)
  log :: String -> m Unit
  ensureCampaign :: Maybe String -> m (Maybe String)
  applyPriceVariations :: (NonEmptyArray UserBucket) -> m Unit
