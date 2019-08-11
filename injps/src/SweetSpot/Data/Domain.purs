module SweetSpot.Data.Domain where

import Data.Newtype (class Newtype)

newtype CampaignId
  = CampaignId String

newtype UserId
  = UserId String

derive instance newtypeUserId :: Newtype UserId _
