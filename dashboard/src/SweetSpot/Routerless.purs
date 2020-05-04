module SweetSpot.Routerless where

import Prelude

import SweetSpot.Data.Api (UICampaign(..))

data Route =
  CampaignList
  | Campaign UICampaign

instance showRoute :: Show Route where
  show CampaignList = "home"
  show (Campaign (UICampaign campaign)) = "campaign: " <> campaign._uiCampaignId

derive instance eqRoute :: Eq Route
