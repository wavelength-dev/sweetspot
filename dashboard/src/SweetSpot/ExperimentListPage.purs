module SweetSpot.ExperimentListPage where

import Prelude
import Data.Array (fold)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime (format) as Formatter
import Data.List (List(..), (:))
import Data.Nullable (notNull)
import Effect (Effect)
import React.Basic.DOM (css, div, li, text, ul) as R
import React.Basic.Hooks (JSX, ReactComponent, component, element)
import SweetSpot.Shopify (button, heading, page) as Shopify

data ExperimentStatus = Draft | Starting DateTime | Running DateTime | Finished DateTime

type ExperimentCardProps
  = { title :: String
    , status :: ExperimentStatus
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
    where formatDate dateTime = fold [
      Formatter.format (MonthShort : Nil) dateTime
      , " "
      , Formatter.format (DayOfMonth : Nil) dateTime
      ]

experimentCard :: ExperimentCardProps -> JSX
experimentCard { status, title } =
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
                [ element Shopify.heading { element: "h2", children: title }
                , R.div
                    { style: R.css { display: "flex", alignItems: "center" }
                    , children: [ experimentStatus status ]
                    }
                ]
            }
        , R.div
            { style: R.css { display: "flex", alignItems: "center" }
            , children: [ element Shopify.button { onClick: mempty, children: "View" } ]
            }
        ]
    }

type ExperimentListPageProps
  = { experiments :: Array ExperimentCardProps
    , onCreateExperiment :: Effect Unit
    }

mkExperimentListPage :: Effect (ReactComponent ExperimentListPageProps)
mkExperimentListPage =
  component "ExperimentListPage" \props ->
    pure
      $ element Shopify.page
          { title: "Price Experiment List"
          , subtitle: notNull "All tests currently running, or finished."
          , primaryAction: notNull { content: "Create Price Experiment", onAction: props.onCreateExperiment }
          , children:
              [ R.ul
                  { children:
                      map
                        ( \cardProps ->
                            R.li
                              { className: "price-experiment-wrapper"
                              , children: [ (experimentCard cardProps) ]
                              }
                        )
                        props.experiments
                  }
              ]
          }
