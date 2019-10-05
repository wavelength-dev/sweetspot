module SweetSpot.Data.Domain where

import Prelude
import Data.Argonaut (caseJsonString, decodeJson, encodeJson) as Argonaut
import Data.Argonaut (class DecodeJson, class EncodeJson, Json)
import Data.Array (find) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)

-- | Id which corresponds to a SweetSpot campaign. Campaigns are time bound events within which price tests take place.
newtype CampaignId
  = CampaignId String

-- | VariantId's are id's used to identify variants.
newtype VariantId
  = VariantId String

derive instance eqVariantId :: Eq VariantId

derive instance ordVariantId :: Ord VariantId

-- | UserIds are id's assigned to users within the SweetSpot system.
newtype UserId
  = UserId String

derive instance newtypeUserId :: Newtype UserId _

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson = unwrap >>> Argonaut.encodeJson

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
    , swapPrice :: Number
    , targetId :: String
    , userId :: String
    }

decodeTestMaps :: Json -> Either String (Array TestMap)
decodeTestMaps json = Argonaut.decodeJson json >>= traverse Argonaut.decodeJson

type TargetId
  = String

findMatchingTestMap :: Array TestMap -> TargetId -> Maybe TestMap
findMatchingTestMap testMaps targetId = Array.find (_.targetId >>> ((==) targetId)) testMaps
