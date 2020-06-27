module SweetSpot.CampaignViewPage where

import Prelude

import Data.Array (zip) as Array
import Data.DateTime (DateTime)
import Data.Formatter.Number (Formatter(..))
import Data.Formatter.Number (format) as Formatter
import Data.Lens (_Just, view, (^.), (^?))
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Maybe as Maybe
import Data.Nullable (notNull, null)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_) as Aff
import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.Basic.DOM (div, p, p_, text) as R
import React.Basic.Hooks (Component, component, element, empty, useState')
import React.Basic.Hooks (bind) as React
import SweetSpot.Data.Api (UICampaign, lowerBound, mean, uiCampaignAOVChange, uiCampaignCRChange, uiCampaignCtrlTreatment, uiCampaignEnd, uiCampaignId, uiCampaignLift, uiCampaignName, uiCampaignStart, uiCampaignTestTreatment, uiTreatmentAOV, uiTreatmentCR, uiTreatmentSku, uiTreatmentVariantPrice, uiTreatmentVariantTitle, uiTreatmentVariants, upperBound)
import SweetSpot.Service (stopCampaign) as Service
import SweetSpot.Session (SessionId)
import SweetSpot.Shopify (card, dataTable, page) as Shopify
import SweetSpot.ShopifyHelper (ElementTag(..))
import SweetSpot.ShopifyHelper (heading, textContainer) as SH
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

numberToDirection :: Number -> Direction
numberToDirection num = if num >= 0.0 then Up else Down

data Size
  = Normal
  | Big

getIndicatorArrow :: Size -> Direction -> JSX
getIndicatorArrow big direction = case direction, big of
  Up, Big -> R.div { className: styles.arrowUpBig }
  Down, Big -> R.div { className: styles.arrowDownBig }
  Up, Normal -> R.div { className: styles.arrowUp }
  Down, Normal -> R.div { className: styles.arrowDown }

resultIndicator :: Maybe String -> String -> Maybe Direction -> JSX
resultIndicator mAmount label directionIndicator =
  let
    amount = Maybe.fromMaybe "--" mAmount
  in
    R.div
      { className: styles.resultIndicator
      , children:
          [ maybe empty (getIndicatorArrow Normal) directionIndicator
          , R.div { className: styles.resultAmount, children: [ R.text amount ] }
          , R.div { className: styles.resultLabel, children: [ R.text label ] }
          ]
      }

mkCampaignViewPage :: Component { campaign :: UICampaign, sessionId :: SessionId }
mkCampaignViewPage = do
  component "CampaignViewPage" \{ campaign, sessionId } -> React.do
    loading /\ setLoading <- useState' false
    let
      campaignId = campaign ^. uiCampaignId

      name = campaign ^. uiCampaignName

      start = campaign ^. uiCampaignStart

      controlConversion = view (uiCampaignCtrlTreatment <<< uiTreatmentCR) campaign

      testConversion = view (uiCampaignTestTreatment <<< uiTreatmentCR) campaign

      averageOrderValueChange = view uiCampaignAOVChange campaign

      conversionChange = view uiCampaignCRChange campaign

      onStopCampaign = liftEffect (setLoading true)
                        *> Service.stopCampaign sessionId campaignId
                        $> setLoading false
                        # Aff.launchAff_
    pure
      $ element Shopify.page
          { title: notNull name
          , subtitle: null
          , breadcrumbs: [ { content: "Campaign list", url: "#/" } ]
          , primaryAction:
              if isJust (campaign ^. uiCampaignEnd)
                 then null
                 else
                  notNull
                    { content: "Stop Experiment"
                    , url: null
                    , primary: false
                    , onAction: notNull onStopCampaign
                    , loading: loading
                    }
          , children:
              [ R.div
                  { className: styles.status
                  , children: [ startingDate start Started, age ]
                  }
              , R.div
                  { className: styles.statusBox
                  , children:
                      [ SH.textContainer
                          [ SH.heading H2 "Revenue per visitor"
                          , R.p_ [ R.text "The answer to the most important question: if you were to switch to your new prices, what would the effect be on the revenue per visitor." ]
                          ]
                      , Spacing.large
                      , R.div
                          { className: styles.revenueResults
                          , children:
                              [ resultIndicator
                                  (campaign ^? _lowerBound <#> formatPercentage)
                                  "lower"
                                  Nothing
                              , meanIndicator campaign
                              , resultIndicator
                                  (campaign ^? _upperBound <#> formatPercentage)
                                  "upper"
                                  Nothing
                              ]
                          }
                      ]
                  }
              , Spacing.large
              , R.div
                  { className: styles.statusBox
                  , children:
                      [ SH.textContainer
                          [ SH.heading H2 "Conversion rate"
                          , R.p_ [ R.text "The percentage of users that buy one of your products under test after landing on your site." ]
                          ]
                      , Spacing.large
                      , R.div
                          { className: styles.revenueResults
                          , children:
                              [ resultIndicator
                                  (controlConversion <#> fractionToPercentage >>> formatPercentage)
                                  "control"
                                  Nothing
                              , resultIndicator
                                  (testConversion <#> fractionToPercentage >>> formatPercentage)
                                  "test"
                                  Nothing
                              , resultIndicator
                                  (conversionChange <#> factorToPercent >>> formatPercentage)
                                  "change"
                                  (conversionChange <#> factorToPercent >>> numberToDirection)
                              ]
                          }
                      ]
                  }
              , Spacing.large
              , R.div
                  { className: styles.statusBox
                  , children:
                      [ SH.textContainer
                          [ SH.heading H2 "Average order value"
                          , R.p_ [ R.text "The size of the average basket users are checking out." ]
                          ]
                      , Spacing.large
                      , R.div
                          { className: styles.revenueResults
                          , children:
                              [ resultIndicator
                                  (campaign ^. _controlAverageOrderValue # Just)
                                  "control"
                                  Nothing
                              , resultIndicator
                                  (campaign ^. _testAverageOrderValue # Just)
                                  "test"
                                  Nothing
                              , resultIndicator
                                  (campaign # getAverageOrderValueChange >>> Just)
                                  "change"
                                  (averageOrderValueChange <#> numberToDirection)
                              ]
                          }
                      ]
                  }
              , Spacing.large
              , element Shopify.card
                  { title: "Products under test"
                  , sectioned: false
                  , children:
                      [ element Shopify.dataTable
                          { columnContentTypes: [ "text", "text", "numeric", "numeric" ]
                          , headings: [ "Product", "Sku", "Control price", "Test price" ]
                          , rows: campaignToProductRows campaign
                          }
                      ]
                  }
              ]
          }
  where
  factorToPercent = (sub 1.0) >>> (mul 100.0)

  _controlVariants = uiCampaignCtrlTreatment <<< uiTreatmentVariants

  _testVariants = uiCampaignTestTreatment <<< uiTreatmentVariants

  _controlAverageOrderValue = uiCampaignCtrlTreatment <<< uiTreatmentAOV

  _testAverageOrderValue = uiCampaignTestTreatment <<< uiTreatmentAOV

  _lowerBound = uiCampaignLift <<< _Just <<< lowerBound

  _upperBound = uiCampaignLift <<< _Just <<< upperBound

  campaignToProductRows campaign =
    let
      controlVariants = view _controlVariants campaign

      testVariants = view _testVariants campaign

      variantPairToRow (Tuple controlVariant testVariant) =
        [ view uiTreatmentVariantTitle controlVariant
        , view uiTreatmentSku controlVariant
        , view uiTreatmentVariantPrice controlVariant
        , view uiTreatmentVariantPrice testVariant
        ]
    in
      map variantPairToRow (Array.zip controlVariants testVariants)

  getAverageOrderValueChange campaign =
      (campaign ^. _controlAverageOrderValue) <> " / " <> (campaign ^. _testAverageOrderValue)

  formatPercentage =
    Formatter.format
      (Formatter { abbreviations: false, after: 1, before: 1, comma: false, sign: true })
      >>> (_ <> "%")

  fractionToPercentage = mul 100.0

  meanIndicator campaign =
    let
      mMean = campaign ^? uiCampaignLift <<< _Just <<< mean :: Maybe Number

      mDirection = mMean <#> numberToDirection

      meanAmount /\ arrow = case mMean of
        Just meanAmount ->
          let
            direction = numberToDirection meanAmount

            arrow = getIndicatorArrow Big direction
          in
            show meanAmount /\ arrow
        Nothing -> "--" /\ mempty
    in
      R.div
        { className: styles.resultIndicator
        , children:
            [ maybe empty (\dir -> getIndicatorArrow Big dir <> Spacing.small) mDirection
            , R.div
                { className: styles.resultAmount__big
                , children: [ R.text meanAmount ]
                }
            , R.div
                { className: styles.resultLabel
                , children: [ R.text "mean" ]
                }
            ]
        }
