module SweetSpot.Main where

import Prelude
import SweetSpot.Data.Api (Product, productTitle)

import Data.Either (Either(..))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (div, text) as R
import React.Basic.DOM (render)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect, useReducer, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.Shopify as Shopify
import SweetSpot.State (Action(..), AppState, fetchAppState, initialState, reducer)
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
          , image: "lol.jpg"
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

toPriceTest :: Product -> PriceTest
toPriceTest product =
  { title: product ^. productTitle
  , creationDate: "Jan 18th"
  , price: "$129"
  , status: "Running"}

experimentsPage :: AppState -> JSX
experimentsPage state =
  element Shopify.page
    { title: "Price Tests"
    , subtitle: notNull "All tests currently running or finished."
    , primaryAction: notNull { content: "Create new price test", onAction: mempty }
    , children:
      [ element Shopify.card
          { sectioned: true
          , children:
            [ element Shopify.resourceList
                { items: map toPriceTest state.products
                , renderItem: renderPriceTest
                }
            ]
          }
      ]
    }


mkApp :: Effect (ReactComponent {})
mkApp =
  component "App" \props -> React.do
    state /\ dispatch <- useReducer initialState reducer

    let
      isExperimentsEmpty = false

    mState <- useAff "appState" fetchAppState

    useEffect (React.UnsafeReference mState) do
      case mState of
        Just (Right st) -> do
          dispatch $ Populate st
          mempty
        _ -> mempty

    pure $
        element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
            if isExperimentsEmpty then
              gettingStartedPage
            else
              experimentsPage state
          }



main :: Effect Unit
main = do
  mAppElement <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> do
      app <- mkApp
      render (element app {}) appElement
