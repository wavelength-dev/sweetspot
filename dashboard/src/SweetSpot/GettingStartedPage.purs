module SweetSpot.GettingStartedPage where

import Prelude

import Assets (mountainCode) as Assets
import Data.Nullable (notNull, null)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (emptyState) as Shopify

gettingStartedPage :: JSX
gettingStartedPage =
  element Shopify.emptyState
    { heading: "Discover more profitable prices for your products"
    , action: notNull { content: "Create Price Experiment", url: null, onAction: notNull $ pure unit }
    , image: Assets.mountainCode
    , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
    }
