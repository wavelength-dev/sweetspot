module Fulcrum.Data where

import Prelude
import Data.Argonaut (caseJsonString, decodeJson) as Argonaut
import Data.Argonaut (class DecodeJson, Json)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | Id which corresponds to a SweetSpot campaign. Campaigns are time bound events within which price tests take place.
newtype CampaignId
  = CampaignId String

-- | VariantId's are id's used to identify variants. Variants are a certain variation of a product and can be bought.
newtype VariantId
  = VariantId String

derive instance eqVariantId :: Eq VariantId

derive instance ordVariantId :: Ord VariantId

newtype Sku
  = Sku String

derive instance eqSku :: Eq Sku

derive instance ordSku :: Ord Sku

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json = Argonaut.caseJsonString (Left "sku is not a string") (Sku >>> Right) json

instance showSku :: Show Sku where
  show (Sku sku) = show sku

-- | A TestMap is a set of data explaining how to control the price of its associated product.
type TestMap
  = { sku :: Sku
    , swapId :: String
    , swapPrice :: String
    , variantId :: String
    , userId :: String
    }

decodeTestMaps :: Json -> Either String (Array TestMap)
decodeTestMaps = Argonaut.decodeJson >=> traverse Argonaut.decodeJson

type TestMapByVariant
  = Map VariantId TestMap

hashMapFromTestMaps :: Array TestMap -> Map VariantId TestMap
hashMapFromTestMaps = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.variantId) testMap
