module SweetSpot.GettingStartedPage where

import Assets (mountainCode) as Assets
import React.Basic.DOM (text) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (emptyState) as Shopify

gettingStartedPage :: JSX
gettingStartedPage =
  element Shopify.emptyState
    { heading: "Discover more profitable prices for your products"
    , action: { content: "Create Price Experiment", url: "#/campaign/create" }
    , image: Assets.mountainCode
    , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
    }
