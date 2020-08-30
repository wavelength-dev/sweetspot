module SweetSpot.CampaignListPage where

import Prelude
import Data.Array (intercalate)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Now (nowDateTime) as Now
import Effect.Timer (clearInterval, setInterval) as Timer
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM (css, div, li, text, ul_) as R
import React.Basic.Hooks (Component, JSX, component, element, useEffect, useState')
import React.Basic.Hooks (bind, discard) as React
import Routing.Hash as Hash
import SweetSpot.Data.Api (UICampaign(..), uiCampaignEnd)
import SweetSpot.Shopify (button, link, modal, modalSection, page) as Shopify
import SweetSpot.ShopifyHelper (ElementTag(..))
import SweetSpot.ShopifyHelper (heading) as SH
import SweetSpot.Spacing as Spacing
import SweetSpot.Date (formatDate)

type Now
  = DateTime

data CampaignStatus
  = Running DateTime
  | Finished DateTime

type CampaignId
  = String

type CampaignEnd
  = DateTime

type CampaignStart
  = DateTime

-- TODO: move this logic to the backend
toStatus :: Now -> Maybe CampaignEnd -> Maybe CampaignStart -> CampaignStatus
toStatus now mEndDateTime mStartDateTime = case mEndDateTime, mStartDateTime of
  -- TODO: update UICampaign to always have start and end
  _, Nothing -> unsafeThrow "Campaign missing start datetime"
  Nothing, Just start -> Running start
  Just end, Just start
    | end < now -> Finished end
    | otherwise -> Running start

campaignToCardProps :: Now -> UICampaign -> CampaignCardProps
campaignToCardProps now campaign'@(UICampaign campaign) =
  { title: campaign._uiCampaignName
  , status: toStatus now campaign._uiCampaignEnd campaign._uiCampaignStart
  , campaignId: campaign._uiCampaignId
  }

spacer :: String -> JSX
spacer size = R.div { style: R.css { width: size, height: size } }

campaignStatus :: CampaignStatus -> JSX
campaignStatus status =
  R.div
    { style:
        R.css
          { padding: "0.25em 1.125em"
          , background:
              case status of
                Running _ -> "#B4E0FA"
                Finished _ -> "#BBE5B3"
          , border: "0.125em solid #FFFFFF"
          , borderRadius: "6.25em"
          , fontColor: "#212B36"
          , fontStyle: "normal"
          , fontWeight: "normal"
          , fontSize: "0.8125em"
          , lineHeight: "1em"
          }
    , children:
        [ R.text case status of
            Running startDateTime -> "Running since " <> formatDate startDateTime
            Finished endDateTime -> "Finished on " <> formatDate endDateTime
        ]
    }

type CampaignCardProps
  = { title :: String
    , status :: CampaignStatus
    , campaignId :: CampaignId
    }

campaignCard :: CampaignCardProps -> JSX
campaignCard { status, title, campaignId } =
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
                [ SH.heading H2 title
                , Spacing.small
                , R.div
                    { style: R.css { display: "flex", alignItems: "center" }
                    , children: [ campaignStatus status ]
                    }
                ]
            }
        , R.div
            { style: R.css { display: "flex", alignItems: "center" }
            , children:
                [ element Shopify.button
                    { url: notNull $ "#/view/" <> campaignId
                    , children: [ R.text "View" ]
                    , submit: false
                    , primary: false
                    , onClick: null
                    , loading: false
                    , disabled: false
                    }
                ]
            }
        ]
    }

checkIsCampaignActive :: DateTime -> UICampaign -> Boolean
checkIsCampaignActive now =
  view uiCampaignEnd
    >>> case _ of
        Nothing -> true
        Just end -> end > now

type CampaignListPageProps
  = { campaigns :: Array UICampaign }

mkCampaignListPage :: Component CampaignListPageProps
mkCampaignListPage =
  component "CampaignListPage" \props -> React.do
    now /\ setNow <- useState' (unsafePerformEffect Now.nowDateTime)
    isSingleCampaignWarningVisible /\ setIsSingleCampaignWarningVisible <- useState' false
    let
      campaigns = map (campaignToCardProps now) props.campaigns

      checkIsCampaignActiveNow = checkIsCampaignActive now

      navigateToCreateOrWarn =
        if Array.any checkIsCampaignActiveNow props.campaigns then
          setIsSingleCampaignWarningVisible true
        else do
          Hash.setHash "/create"
    useEffect unit do
      intervalId <- Timer.setInterval 1000 (Now.nowDateTime >>= setNow)
      pure $ Timer.clearInterval intervalId
    pure
      $ element Shopify.page
          { title: notNull "Price Experiment List"
          , subtitle: notNull "Shown here are all price experiments currently running, or previously finished."
          , primaryAction:
              notNull
                { content: "Create Price Experiment"
                , url: null
                , primary: true
                , onAction: notNull navigateToCreateOrWarn
                , loading: false
                }
          , breadcrumbs: []
          , children:
              [ element Shopify.modal
                  { title: "Not yet!"
                  , open: isSingleCampaignWarningVisible
                  , onClose: setIsSingleCampaignWarningVisible false # const >>> mkEffectFn1
                  , children:
                      [ element Shopify.modalSection
                          { key: "warning-section"
                          , children:
                              [ R.text
                                  "Currently, SweetSpot only supports running a single campaign at a time. Either wait for the running experiment to end, or stop it. If you'd like to run multiple campaigns at the same time, let us know: "
                              , element Shopify.link
                                  { url: "mailto:hello@wavelength.dev"
                                  , children: [ R.text "hello@wavelength.dev" ]
                                  }
                              ]
                          }
                      ]
                  }
              , R.ul_ [ props.campaigns # map (toCard now) >>> (intercalate Spacing.medium) ]
              ]
          }
  where
  toCard now (UICampaign campaign) =
    R.li
      { className: "price-experiment-wrapper"
      , children:
          [ campaignCard
              { campaignId: campaign._uiCampaignId
              , status: toStatus now campaign._uiCampaignEnd campaign._uiCampaignStart
              , title: campaign._uiCampaignName
              }
          ]
      }
