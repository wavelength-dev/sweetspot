module SweetSpot.GettingStartedPage where

import Prelude
import Assets (mountainCode) as Assets
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (emptyState) as Shopify
import SweetSpot.State (AppState)

gettingStartedPage :: AppState -> JSX
gettingStartedPage { shopName } =
  let
    heading = case shopName of
      Just name -> "Hi " <> name <> ", Discover more profitable prices for your products"
      Nothing -> "Discover more profitable prices for your products"
  in
    element Shopify.emptyState
      { heading: notNull heading
      , action: notNull { content: "Create Price Experiment", onAction: mempty }
      , image: Assets.mountainCode
      , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
      }
