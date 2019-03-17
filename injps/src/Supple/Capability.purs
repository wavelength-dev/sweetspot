module Supple.Capability where

import Prelude

import Data.Maybe (Maybe)
import Supple.Data.Api (UserBucket)

class Monad m <= AppCapability m where
  ensureDeps :: m Unit
  getUserId :: m (Maybe String)
  setUserId :: UserBucket -> m Unit
  getUserBucket :: Maybe String -> m UserBucket
  log :: String -> m Unit
