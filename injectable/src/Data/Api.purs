module SweetSpot.Data.Api where

import Prelude
import Data.Argonaut (Json, decodeJson)
import Data.Either (Either)
import Data.Traversable (traverse)
import SweetSpot.Data.Domain (Sku)

type TestMap
  = { sku :: Sku
    , swapId :: String
    , swapPrice :: Int
    , targetId :: String
    , userId :: String
    }

decodeTestMaps :: Json -> Either String (Array TestMap)
decodeTestMaps json = decodeJson json >>= traverse decodeJson
