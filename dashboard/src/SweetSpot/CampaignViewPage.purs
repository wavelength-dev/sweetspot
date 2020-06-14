module SweetSpot.CampaignViewPage where

import Prelude
import Data.Array (zip) as Array
import Data.DateTime (DateTime)
import Data.Formatter.Number (Formatter(..))
import Data.Formatter.Number (format) as Formatter
import Data.Lens (view)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (notNull, null)
import Data.Tuple (Tuple(..))
import React.Basic (JSX)
import React.Basic.DOM (div, p, p_, text) as R
import React.Basic.Hooks (Component, component, element, empty)
import SweetSpot.Data.Api (UICampaign, lowerBound, mean, uiCampaignAOVChange, uiCampaignCRChange, uiCampaignCtrlTreatment, uiCampaignLift, uiCampaignName, uiCampaignStart, uiCampaignTestTreatment, uiTreatmentAOV, uiTreatmentCR, uiTreatmentSku, uiTreatmentVariantPrice, uiTreatmentVariantTitle, uiTreatmentVariants, upperBound)
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

getIndicatorArrow :: Boolean -> Direction -> JSX
getIndicatorArrow big direction = case direction, big of
  Up, true -> R.div { className: styles.arrowUpBig }
  Down, true -> R.div { className: styles.arrowDownBig }
  Up, false -> R.div { className: styles.arrowUp }
  Down, false -> R.div { className: styles.arrowDown }

resultIndicator :: String -> String -> Maybe Direction -> JSX
resultIndicator amount label directionIndicator =
  R.div
    { className: styles.resultIndicator
    , children:
        [ maybe empty (getIndicatorArrow false) directionIndicator
        , R.div { className: styles.resultAmount, children: [ R.text amount ] }
        , R.div { className: styles.resultLabel, children: [ R.text label ] }
        ]
    }

mkCampaignViewPage :: Component { campaign :: UICampaign }
mkCampaignViewPage =
  component "CampaignViewPage" \{ campaign } -> React.do
    let
      name = view uiCampaignName campaign

      start = view uiCampaignStart campaign

      controlConversion = view (uiCampaignCtrlTreatment <<< uiTreatmentCR) campaign

      testConversion = view (uiCampaignTestTreatment <<< uiTreatmentCR) campaign

      averageOrderValueChange = view uiCampaignAOVChange campaign

      conversionChange = view uiCampaignCRChange campaign
    pure
      $ element Shopify.page
          { title: notNull name
          , subtitle: null
          , breadcrumbs: [ { content: "campaign list", url: "#/" } ]
          , primaryAction: null
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
                                  (controlConversion # toPercentage >>> formatPercentage)
                                  "control"
                                  Nothing
                              , resultIndicator
                                  (testConversion # toPercentage >>> formatPercentage)
                                  "test"
                                  Nothing
                              , resultIndicator
                                  (conversionChange # factorToPercent >>> formatPercentage)
                                  "change"
                                  (conversionChange # factorToPercent >>> numberToDirection >>> Just)
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
                                  (campaign # view controlAverageOrderValueOptic)
                                  "control"
                                  Nothing
                              , resultIndicator
                                  (campaign # view testAverageOrderValueOptic)
                                  "test"
                                  Nothing
                              , resultIndicator
                                  (campaign # getAverageOrderValueChange)
                                  "change"
                                  (averageOrderValueChange # numberToDirection >>> Just)
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

  controlVariantsOptic = uiCampaignCtrlTreatment <<< uiTreatmentVariants

  testVariantsOptic = uiCampaignTestTreatment <<< uiTreatmentVariants

  campaignToProductRows campaign =
    let
      controlVariants = view controlVariantsOptic campaign

      testVariants = view testVariantsOptic campaign

      variantPairToRow (Tuple controlVariant testVariant) =
        [ view uiTreatmentVariantTitle controlVariant
        , view uiTreatmentSku controlVariant
        , view uiTreatmentVariantPrice controlVariant
        , view uiTreatmentVariantPrice testVariant
        ]
    in
      map variantPairToRow (Array.zip controlVariants testVariants)

  controlAverageOrderValueOptic = uiCampaignCtrlTreatment <<< uiTreatmentAOV

  testAverageOrderValueOptic = uiCampaignTestTreatment <<< uiTreatmentAOV

  getAverageOrderValueChange campaign =
    let
      controlAverageOrderValue = view controlAverageOrderValueOptic campaign

      testAverageOrderValue = view testAverageOrderValueOptic campaign
    in
      (view controlAverageOrderValueOptic campaign) <> " / " <> (view testAverageOrderValueOptic campaign)

  getMean = view (uiCampaignLift <<< mean) >>> formatPercentage

  getLowerBound = view (uiCampaignLift <<< lowerBound) >>> formatPercentage

  getUpperBound = view (uiCampaignLift <<< upperBound) >>> formatPercentage

  getDirection = view (uiCampaignLift <<< mean) >>> numberToDirection

  formatPercentage =
    Formatter.format
      (Formatter { abbreviations: false, after: 1, before: 1, comma: false, sign: true })
      >>> (_ <> "%")

  toPercentage num = num * 100.0

  meanIndicator amount label direction =
    R.div
      { className: styles.resultIndicator
      , children:
          [ getIndicatorArrow true direction
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
