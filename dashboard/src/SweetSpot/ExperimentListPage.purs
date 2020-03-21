module SweetSpot.ExperimentListPage where

import Prelude
import Data.Nullable (notNull)
import Effect (Effect)
import React.Basic.DOM (css, div, li, text, ul) as R
import React.Basic.Hooks (JSX, ReactComponent, component, element)
import SweetSpot.Shopify (button, heading, page, subheading) as Shopify

data ExperimentStatus
  = Running
  | Done

type ExperimentCardProps
  = { title :: String
    , creationDate :: String
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
                Running -> "#B4E0FA"
                Done -> "#BBE5B3"
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
            Running -> "Running"
            Done -> "Done"
        ]
    }

experimentCard :: ExperimentCardProps -> JSX
experimentCard { creationDate, status, title } =
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
                    , children:
                        [ element Shopify.subheading { element: "h3", children: creationDate }
                        , spacer "1.125em"
                        , experimentStatus status
                        ]
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
