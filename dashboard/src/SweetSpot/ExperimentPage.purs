module SweetSpot.ExperimentPage where

import Prelude

import Data.Newtype (unwrap)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (Component, component)
import SweetSpot.Data.Api (UICampaign)

mkExperimentPage :: Component { campaign :: UICampaign }
mkExperimentPage =
  component "ExperimentPage" \props -> React.do
    pure $ R.text (unwrap props.campaign)._uiCampaignName
