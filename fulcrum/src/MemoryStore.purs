module Fulcrum.MemoryStore where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect (Effect)
import Fulcrum.Data (TestMap, VariantId)

foreign import get_ :: Effect MemoryStore

foreign import set_ :: MemoryStore -> Effect Unit

type CachedTestContext
  = { created :: Instant, variantTestMap :: Map VariantId TestMap }

type MemoryStore
  = { mCachedTestContext :: Maybe CachedTestContext }

get :: Effect MemoryStore
get = get_

set :: MemoryStore -> Effect Unit
set = set_
