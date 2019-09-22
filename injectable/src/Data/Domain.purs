module SweetSpot.Data.Domain where

import Prelude
import Data.Argonaut (caseJsonString, decodeJson, encodeJson) as Argonaut
import Data.Argonaut (class DecodeJson, class EncodeJson, Json)
import Data.Array (find) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- | Id which corresponds to a SweetSpot campaign. Campaigns are time bound events within which price tests take place.
newtype CampaignId
  = CampaignId String

-- | Svid's are id's used to identify variants.
newtype Svid
  = Svid String

-- | UserIds are id's assigned to users within the SweetSpot system.
newtype UserId
  = UserId String

derive instance newtypeUserId :: Newtype UserId _

instance encodeJsonUserId :: EncodeJson UserId where
  encodeJson = unwrap >>> Argonaut.encodeJson

newtype Sku
  = Sku String

derive instance eqSku :: Eq Sku

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json = Argonaut.caseJsonString (Left "sku is not a string") (Sku >>> Right) json

instance showSku :: Show Sku where
  show (Sku sku) = show sku

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

type TestMapsMap
  = Map String TestMap

getTestMapsByTargetId :: NonEmptyArray TestMap -> TestMapsMap
getTestMapsByTargetId = map (\testMap -> Tuple testMap.targetId testMap) >>> Map.fromFoldable
