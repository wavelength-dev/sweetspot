module SweetSpot.Capability where

import Prelude

import Data.Maybe (Maybe)
import SweetSpot.Data.Api (UserBucket)

class Monad m <= AppCapability m where
  ensureDeps :: m Unit
  getUserId :: m (Maybe String)
  setUserId :: UserBucket -> m Unit
  getUserBucket :: Maybe String -> Maybe String -> m UserBucket
  log :: String -> m Unit
  ensureCampaign :: Maybe String -> m (Maybe String)
