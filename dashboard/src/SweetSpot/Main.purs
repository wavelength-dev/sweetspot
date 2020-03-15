module SweetSpot.Main where

import Prelude

import Data.Either (Either(..))
import Data.Lens (_Right, findOf, folded, (^.))
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect, useReducer, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import SweetSpot.Data.Api (Product, productTitle)
import SweetSpot.QueryString (QueryParam(..))
import SweetSpot.QueryString as QS
import SweetSpot.Route (Route(..), hoistRouter)
import SweetSpot.Shopify as Shopify
import SweetSpot.State (Action(..), AppState, fetchRemoteState, initialState, reducer, Dispatch)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (search)
import Web.HTML.Window (document, location)

gettingStartedPage :: String -> JSX
gettingStartedPage name =
  element Shopify.page
    { title: "Getting started"
    , subtitle: null
    , primaryAction: null
    , children:
        [ element Shopify.emptyState
            { heading: "Hi " <> name <> ", Discover more profitable prices for your products"
            , action: { content: "Create Price Test", onAction: mempty }
            , image: "lol.jpg"
            , children: [ R.text "Here you'll create new price tests, check their progress, or their outcome." ]
            }
        , R.a
            { href: "/#/"
            , children: [ R.button { children: [ R.text "Click here" ] } ]
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
  , status: "Running"
  }

experimentsPage :: AppState -> Dispatch -> JSX
experimentsPage state dispatch =
  element Shopify.page
    { title: "Price Tests"
    , subtitle: notNull "All tests currently running or finished."
    , primaryAction: notNull { content: "Create new price test", onAction: dispatch (Navigate (Campaign "lol")) }
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
mkApp = do
  qs <- window >>= location >>= search <#> QS.parseQueryString

  let
    apiKey = "634b531a6568d6eb076c2ad5c7e0265a"
    mShopDomain = findOf (folded <<< _Right) (\(QueryParam k _) -> k == "shop") qs
      >>= (\(QueryParam _ v) -> v)

  app <- case mShopDomain of
    Just domain -> Shopify.createApp apiKey domain
    Nothing -> throw "Failed to parse shop domain"

  component "App" \props -> React.do
    state /\ dispatch <- useReducer initialState reducer
    mState <- useAff "appState" fetchRemoteState
    useEffect "router" $ hoistRouter (dispatch <<< Navigate)
    useEffect (React.UnsafeReference mState) do
      case mState of
        Just (Right st) -> do
          dispatch $ Populate st
          mempty
        _ -> mempty
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              case state.route of
                Home -> experimentsPage state dispatch
                _ -> experimentsPage state dispatch
          }

main :: Effect Unit
main = do
  mAppElement <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> do
      app <- mkApp
      render (element app {}) appElement
