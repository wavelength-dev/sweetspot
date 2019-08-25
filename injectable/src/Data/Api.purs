module SweetSpot.Data.Api where

import Prelude
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either)
import Data.Traversable (traverse)

type TestMap
  = { sku :: String
    , swapId :: Number
    , swapPrice :: Number
    , targetId :: Number
    , userId :: Number
    }

-- instance decodeTestMap :: DecodeJson TestMap where
--   decodeJson json = do
--     x <- decodeJson json
--     sku <- x .: "sku"
--     swapId <- x .: "swapId"
--     swapPrice <- x .: "swapPrice"
--     targetId <- x .: "targetId"
--     userId <- x .: "userId"
--     pure $ TestMap { sku, swapId, swapPrice, targetId, userId }

decodeTestMaps :: Json -> Either String (Array TestMap)
decodeTestMaps json =
  decodeJson json
    >>= traverse \jsonTestMap -> do
        x <- decodeJson jsonTestMap
        sku <- x .: "sku"
        swapId <- x .: "swapId"
        swapPrice <- x .: "swapPrice"
        targetId <- x .: "targetId"
        userId <- x .: "userId"
        pure $ { sku, swapId, swapPrice, targetId, userId }
