module SweetSpot.ExperimentPage where

import Prelude

import Data.Array (fold)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (format)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import React.Basic (JSX)
import React.Basic.DOM (div, p, p_, text) as R
import React.Basic.Hooks (Component, component, element)
import SweetSpot.Data.Api (UICampaign(..), lowerBound, mean, uiCampaignLift, uiCampaignName, upperBound)
import SweetSpot.Shopify (page, textContainer_) as Shopify
import SweetSpot.ShopifyWrapper (Element(..))
import SweetSpot.ShopifyWrapper (heading) as ShopifyWrapper
import SweetSpot.Spacing (large, small) as Spacing

foreign import styles :: forall a. Record a

type CampaignStart
  = DateTime

data CreatedExperimentStatus
  = Starting
  | Started
  | Finished

startingDate :: Maybe CampaignStart -> CreatedExperimentStatus -> JSX
startingDate dateTime status =
  R.div
    { className: styles.statusItem
    , children:
        [ R.p { className: styles.statusItem__title, children: [ R.text "Jan 23rd" ] }
        , Spacing.small
        , R.p { className: styles.statusItem__subtitle, children: [ R.text "started" ] }
        ]
    }

age :: JSX
age =
  R.div
    { className: styles.statusItem
    , children:
        [ R.p { className: styles.statusItem__title, children: [ R.text "22 days" ] }
        , Spacing.small
        , R.p { className: styles.statusItem__subtitle, children: [ R.text "age" ] }
        ]
    }

data Direction
  = Up
  | Down

toDirection :: Number -> Direction
toDirection num = if num >= 0.0 then Up else Down

arrowUp :: JSX
arrowUp = R.div { className: styles.arrowUp }

arrowDown :: JSX
arrowDown = R.div { className: styles.arrowDown }

resultIndicator :: String -> String -> Maybe Direction -> JSX
resultIndicator amount label directionIndicator =
  R.div
    { className: styles.resultIndicator
    , children:
        [ indicatorArrow directionIndicator
        , Spacing.small
        , R.div { className: styles.resultAmount, children: [ R.text amount ] }
        , R.div { className: styles.resultLabel, children: [ R.text label ] }
        ]
    }
  where
  indicatorArrow = case _ of
    Nothing -> mempty
    Just Up -> arrowUp
    Just Down -> arrowDown

mkExperimentPage :: Component { campaign :: UICampaign }
mkExperimentPage =
  component "ExperimentPage" \{ campaign } -> React.do
    let
      (UICampaign c) = campaign
    pure
      $ element Shopify.page
          { title: c._uiCampaignName
          , subtitle: null
          , breadcrumbs: [ { content: "Experiment List", url: "#campaign-list" } ]
          , primaryAction: null
          , children:
              fold
                [ R.div
                    { className: styles.status
                    , children: [ startingDate c._uiCampaignStart Started, age ]
                    }
                , R.div
                    { className: styles.statusBox
                    , children:
                        [ Shopify.textContainer_
                            $ fold
                                [ ShopifyWrapper.heading { element: H2, text: "Revenue per visitor" }
                                , R.p_ [ R.text "The answer to the most important question: if you were to switch to your new prices, what would the effect be on the revenue per visitor." ]
                                ]
                        , Spacing.large
                        , R.div
                            { className: styles.revenueResults
                            , children:
                                [ resultIndicator
                                    (getLowerBound campaign)
                                    "lower"
                                    Nothing
                                , meanIndicator
                                    (getMean campaign)
                                    "mean"
                                    (getDirection campaign)
                                , resultIndicator
                                    (getUpperBound campaign)
                                    "upper"
                                    Nothing
                                ]
                            }
                        ]
                    }
                ]
          }
  where
  getMean = view (uiCampaignLift <<< mean) >>> formatPercentage

  getLowerBound = view (uiCampaignLift <<< lowerBound) >>> formatPercentage

  getUpperBound = view (uiCampaignLift <<< upperBound) >>> formatPercentage

  getDirection = view (uiCampaignLift <<< mean) >>> toDirection >>> Just

  formatPercentage num = getDirectionSign num <> show num <> "%"

  getDirectionSign num = if num >= 0.0 then "+" else "-"

  meanIndicator amount label direction =
    R.div
      { className: styles.resultIndicator
      , children:
          [ indicatorArrow direction
          , Spacing.small
          , R.div
              { className: styles.resultAmount__big
              , children: [ R.text amount ]
              }
          , R.div
              { className: styles.resultLabel
              , children: [ R.text label ]
              }
          ]
      }
    where
    indicatorArrow = case _ of
      Nothing -> mempty
      Just Up -> arrowUp
      Just Down -> arrowDown
