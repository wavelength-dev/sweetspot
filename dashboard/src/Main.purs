module Main where

import Prelude
import Assets (mountainCodeSvg)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (div, text) as R
import React.Basic.DOM (render)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Route (Route(..), hoistRouter)
import Shopify as Shopify
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

gettingStartedPage :: JSX
gettingStartedPage =
  element Shopify.page
    { title: "Getting started"
    , subtitle: null
    , primaryAction: null
    , children:
      [ element Shopify.emptyState
          { heading: "Discover more profitable prices for your products"
          , action: { content: "Create Price Test", onAction: mempty }
          , image: mountainCodeSvg
          , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
          }
      ]
    }

type PriceTest
  = { title :: String
    , creationDate :: String
    , price :: String
    , status :: String
    }

renderPriceTest :: PriceTest -> JSX
renderPriceTest { title } =
  R.div
    { children:
      [ element Shopify.heading { element: "h2", children: title }
      ]
    }

experimentsPage :: JSX
experimentsPage =
  element Shopify.page
    { title: "Price Tests"
    , subtitle: notNull "All tests currently running or finished."
    , primaryAction: notNull { content: "Create new price test", onAction: mempty }
    , children:
      [ element Shopify.card
          { sectioned: true
          , children:
            [ element Shopify.resourceList
                { items:
                  [ { title: "The Piston Jacket", creationDate: "Jan 18th", price: "$129", status: "Running"
                    }
                  ]
                , renderItem: renderPriceTest
                }
            ]
          }
      ]
    }

mkApp :: Effect (ReactComponent {})
mkApp = do
  component "App" \props -> React.do
    counter /\ setCounter <- useState 0
    route /\ setRoute <- useState Home
    useEffect unit (hoistRouter setRoute)
    let
      handleOnClick = setCounter $ add 1

      isExperimentsEmpty = false
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
            if isExperimentsEmpty then
              gettingStartedPage
            else
              experimentsPage
          }

main :: Effect Unit
main = do
  mAppElement <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> do
      app <- mkApp
      render (element app {}) appElement
