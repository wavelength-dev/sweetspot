module SweetSpot.ExperimentListPage where

import Prelude

import Data.Array (fold)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime (format) as Formatter
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Now (nowDateTime) as Now
import Effect.Timer (clearInterval, setInterval) as Timer
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM (css, div, li, text, ul) as R
import React.Basic.Hooks (Component, JSX, component, element, useEffect, useState)
import React.Basic.Hooks (bind, discard) as React
import SweetSpot.Data.Api (UICampaign(..))
import SweetSpot.Shopify (button, heading, page) as Shopify

type Now
  = DateTime

data ExperimentStatus
  = Draft
  | Starting DateTime
  | Running DateTime
  | Finished DateTime

type ExperimentId
  = String

type ExperimentCardProps
  = { id :: ExperimentId
    , title :: String
    , status :: ExperimentStatus
    , onViewCampaign :: Effect Unit
    }

type CampaignEnd
  = DateTime

type CampaignStart
  = DateTime

-- TODO: move this logic to the backend
toStatus :: Now -> Maybe CampaignEnd -> Maybe CampaignStart -> ExperimentStatus
toStatus now endDateTime startDateTime = case endDateTime, startDateTime of
  Nothing, Nothing -> Draft
  Just end, Nothing -> Draft
  Just end, Just start
    | end < now -> Finished end
    | otherwise -> toStatus now Nothing startDateTime
  Nothing, Just start
    | start < now -> Running start
    | otherwise -> Starting start

campaignToCardProps :: Now -> (String -> Effect Unit) -> UICampaign -> ExperimentCardProps
campaignToCardProps now onViewCampaignByCampaign campaign'@(UICampaign campaign) =
  { id: campaign._uiCampaignId
  , title: campaign._uiCampaignName
  , status: toStatus now campaign._uiCampaignEnd campaign._uiCampaignStart
  , onViewCampaign: onViewCampaignByCampaign campaign._uiCampaignId
  }

spacer :: String -> JSX
spacer size = R.div { style: R.css { width: size, height: size } }

experimentStatus :: ExperimentStatus -> JSX
experimentStatus status =
  R.div
    { style:
        R.css
          { padding: "0.25em 1.125em"
          , background:
              case status of
                Draft -> "#DFE3E8"
                Starting _ -> "#DFE3E8"
                Running _ -> "#B4E0FA"
                Finished _ -> "#BBE5B3"
          , border: "0.125em solid #FFFFFF"
          , borderRadius: "6.25em"
          , fontFamily: "SF Pro Text"
          , fontColor: "#212B36"
          , fontStyle: "normal"
          , fontWeight: "normal"
          , fontSize: "0.8125em"
          , lineHeight: "1em"
          }
    , children:
        [ R.text case status of
            Draft -> "Running"
            Starting startDateTime -> "Starting on " <> formatDate startDateTime
            Running startDateTime -> "Running since " <> formatDate startDateTime
            Finished endDateTime -> "Finished on " <> formatDate endDateTime
        ]
    }
  where
  formatDate dateTime =
    fold
      [ Formatter.format (MonthShort : Nil) dateTime
      , " "
      , Formatter.format (DayOfMonth : Nil) dateTime
      ]

experimentCard :: ExperimentCardProps -> JSX
experimentCard { id, status, title, onViewCampaign } =
  R.div
    { className: "price-experiment"
    , style:
        R.css
          { display: "flex"
          , background: "white"
          , flexDirection: "row"
          , justifyContent: "space-between"
          , padding: "1.25em"
          , borderRadius: "3px"
          , boxShadow: "0px 1px 3px rgba(63, 63, 68, 0.15)"
          }
    , children:
        [ R.div
            { style:
                R.css
                  { display: "flex"
                  , flexDirection: "column"
                  , justifyContent: "space-between"
                  }
            , children:
                [ element Shopify.heading { element: "h2", children: R.text title }
                , R.div
                    { style: R.css { display: "flex", alignItems: "center" }
                    , children: [ experimentStatus status ]
                    }
                ]
            }
        , R.div
            { style: R.css { display: "flex", alignItems: "center" }
            , children: [ element Shopify.button { onClick: onViewCampaign, children: R.text "View" } ]
            }
        ]
    }

type ExperimentListPageProps
  = { campaigns :: Array UICampaign
    , onViewCampaignByCampaign :: String -> Effect Unit
    , onCreateExperiment :: Effect Unit
    }

mkExperimentListPage :: Component ExperimentListPageProps
mkExperimentListPage =
  component "ExperimentListPage" \props -> React.do
    now /\ setNow <- useState (unsafePerformEffect Now.nowDateTime)
    let
      campaigns = map (campaignToCardProps now props.onViewCampaignByCampaign) props.campaigns

      setNow' = const >>> setNow
    useEffect unit do
      intervalId <- Timer.setInterval 1000 (Now.nowDateTime >>= setNow')
      pure $ Timer.clearInterval intervalId
    pure
      $ element Shopify.page
          { title: "Price Experiment List"
          , subtitle: notNull "All tests currently running, or finished."
          , primaryAction: notNull { content: "Create Price Experiment", onAction: props.onCreateExperiment }
          , breadcrumbs: []
          , children:
              R.ul
                { children:
                    map
                      ( \cardProps ->
                          R.li
                            { className: "price-experiment-wrapper"
                            , children: [ experimentCard cardProps ]
                            }
                      )
                      campaigns
                }
          }
