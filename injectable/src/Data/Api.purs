module SweetSpot.Data.Api where

import Prelude ((>>=))

import Data.Argonaut (Json, decodeJson)
import Data.Either (Either)
import Data.Traversable (traverse)

type TestMap
  = { sku :: String
    , swapId :: Number
    , swapPrice :: Number
    , targetId :: Number
    , userId :: Number
    }

decodeTestMaps :: Json -> Either String (Array TestMap)
decodeTestMaps json = decodeJson json >>= traverse decodeJson
