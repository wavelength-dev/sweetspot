module Fulcrum.Data where

import Prelude
import Data.Argonaut (caseJsonString, decodeJson, fromString, toString) as Argonaut
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Newtype (class Newtype)
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

derive instance newtypeSku :: Newtype Sku _

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json =
    Argonaut.toString json
      # note (TypeMismatch "sku is not a string")
      >>> map Sku

instance encodeJsonSku :: EncodeJson Sku where
  encodeJson (Sku sku) = Argonaut.fromString sku

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

decodeTestMaps :: Json -> Either JsonDecodeError (Array TestMap)
decodeTestMaps = Argonaut.decodeJson >=> traverse Argonaut.decodeJson

type TestMapByVariant
  = Map VariantId TestMap

hashMapFromTestMaps :: Array TestMap -> Map VariantId TestMap
hashMapFromTestMaps = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.variantId) testMap
