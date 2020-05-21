module SweetSpot.CampaignViewPage where

import Prelude
import Data.Array (fold)
import Data.Array (zip) as Array
import Data.DateTime (DateTime)
import Data.Formatter.Number (Formatter(..))
import Data.Formatter.Number (format) as Formatter
import Data.Lens (view)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Tuple (Tuple(..))
import React.Basic (JSX)
import React.Basic.DOM (div, p, p_, text) as R
import React.Basic.Hooks (Component, component, element, empty)
import SweetSpot.Data.Api (UICampaign(..), lowerBound, mean, uiCampaignCtrlTreatment, uiCampaignLift, uiCampaignTestTreatment, uiTreatmentAOV, uiTreatmentCR, uiTreatmentSku, uiTreatmentVariantPrice, uiTreatmentVariantTitle, uiTreatmentVariants, upperBound)
import SweetSpot.Shopify (card, dataTable, page, textContainer_) as Shopify
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
  component "CampaignPage" \{ campaign } -> React.do
    let
      (UICampaign c) = campaign
    pure
      $ element Shopify.page
          { title: c._uiCampaignName
          , subtitle: null
          , breadcrumbs: [ { content: "campaign List", url: "#/" } ]
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
                                , R.p_ [ R.text "The percentage of users that buy one of your products under test." ]
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
                        [ Shopify.textContainer_
                            $ fold
                                [ ShopifyWrapper.heading { element: H2, text: "Conversion rate" }
                                , R.p_ [ R.text "The percentage of users that buy one of your products under test after landing on your site." ]
                                ]
                        , Spacing.large
                        , R.div
                            { className: styles.revenueResults
                            , children:
                                [ resultIndicator
                                    (campaign # view controlConversionOptic >>> toPercentage >>> formatPercentage)
                                    "control"
                                    Nothing
                                , resultIndicator
                                    (campaign # view testConversionOptic >>> toPercentage >>> formatPercentage)
                                    "test"
                                    Nothing
                                , resultIndicator
                                    (getConversionChange campaign # toPercentage >>> formatPercentage)
                                    "change"
                                    (numberToDirection (getConversionChange campaign) # Just)
                                ]
                            }
                        ]
                    }
                , Spacing.large
                , R.div
                    { className: styles.statusBox
                    , children:
                        [ Shopify.textContainer_
                            $ fold
                                [ ShopifyWrapper.heading { element: H2, text: "Average order value" }
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
                                    (numberToDirection (getConversionChange campaign) # Just)
                                ]
                            }
                        ]
                    }
                , Spacing.large
                , element Shopify.card
                    { title: "Products under test"
                    , sectioned: false
                    , children:
                        fold
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

  controlConversionOptic = uiCampaignCtrlTreatment <<< uiTreatmentCR

  testConversionOptic = uiCampaignTestTreatment <<< uiTreatmentCR

  getConversionChange campaign =
    let
      controlConversion = view controlConversionOptic campaign

      testConversion = view testConversionOptic campaign
    in
      (testConversion - controlConversion) / controlConversion

  controlAverageOrderValueOptic = uiCampaignCtrlTreatment <<< uiTreatmentAOV

  testAverageOrderValueOptic = uiCampaignTestTreatment <<< uiTreatmentAOV

  getAverageOrderValueChange campaign =
    let
      controlAverageOrderValue = view controlAverageOrderValueOptic campaign

      testAverageOrderValue = view testAverageOrderValueOptic campaign
    in
      controlAverageOrderValue <> " / " <> testAverageOrderValue

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
