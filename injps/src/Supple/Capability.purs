module Supple.Capability where

import Prelude

import Data.Maybe (Maybe)
import Supple.Data.Api (UserBucket)

class Monad m <= AppCapability m where
  getUserId :: m (Maybe String)
  setUserId :: String -> m Unit
  getUserBuckets :: Maybe String -> m UserBucket
  log :: String -> m Unit
