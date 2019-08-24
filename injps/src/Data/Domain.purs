module SweetSpot.Data.Domain where

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
