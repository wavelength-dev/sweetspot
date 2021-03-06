module SweetSpot.GettingStartedPage where

import Assets (mountainCode) as Assets
import Data.Nullable (notNull)
import React.Basic.DOM (text, p, a, css) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (emptyState) as Shopify

gettingStartedPage :: JSX
gettingStartedPage =
  element Shopify.emptyState
    { heading: "Discover more profitable prices for your products"
    , action: notNull { content: "Create Price Experiment", url: "#/create" }
    , image: Assets.mountainCode
    , children:
        [ R.p
            { style: R.css { marginTop: "1em" }
            , children: [ R.text "For SweetSpot to work, minor changes need to be made to your theme first. Before creating your first experiment, please make sure to read the ", R.a { target: "_blank", href: "https://getsweetspot.com/getting-started", children: [ R.text "getting started guide." ] } ]
            }
        ]
    }
