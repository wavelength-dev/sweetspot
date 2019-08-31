module SweetSpot.Data.Domain where

import Prelude
import Data.Argonaut (class DecodeJson, caseJsonString)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)

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

newtype Sku
  = Sku String

derive instance eqSku :: Eq Sku

instance decodeJsonSku :: DecodeJson Sku where
  decodeJson json = caseJsonString (Left "sku is not a string") (Right <<< Sku) json

instance showSku :: Show Sku where
  show (Sku sku) = show sku
