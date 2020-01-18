module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (ReactComponent, component, element, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Route (Route(..), hoistRouter)
import Shopify as Shopify
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

mkApp :: Effect (ReactComponent {})
mkApp = do
  component "App" \props -> React.do
    counter /\ setCounter <- useState 0
    route /\ setRoute <- useState Home
    useEffect unit (hoistRouter setRoute)
    let
      handleOnClick = setCounter $ add 1
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
            [ element Shopify.page
                { title: show route
                , children:
                  [ element Shopify.card
                      { sectioned: true
                      , children:
                        [ element Shopify.button
                            { onClick: handleOnClick
                            , children: [ R.text ("Increase count: " <> show counter) ]
                            }
                        ]
                      }
                  ]
                }
            ]
          }

main :: Effect Unit
main = do
  mAppElement <- getElementById "app" =<< (map toNonElementParentNode $ document =<< window)
  case mAppElement of
    Nothing -> throw "app element not found."
    Just appElement -> do
      app <- mkApp
      render (element app {}) appElement
