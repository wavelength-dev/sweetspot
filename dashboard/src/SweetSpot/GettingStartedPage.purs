module SweetSpot.GettingStartedPage where

import Prelude
import Assets (mountainCode) as Assets
import Data.Nullable (notNull)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (emptyState) as Shopify

gettingStartedPage :: JSX
gettingStartedPage =
  element Shopify.emptyState
    { heading: notNull "Discover more profitable prices for your products"
    , action: notNull { content: "Create Price Experiment", onAction: mempty }
    , image: Assets.mountainCode
    , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
    }
