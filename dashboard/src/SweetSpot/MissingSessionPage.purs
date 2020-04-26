module SweetSpot.MissingSessionPage where

import Prelude
import Data.Nullable (notNull, null)
import Effect (Effect)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (Component, component, element)
import SweetSpot.Shopify (appProvider, emptyState, enTranslations) as Shopify

mkMissingSessionPage :: Component {}
mkMissingSessionPage =
  component "MissingSessionPage" \_ ->
    pure
      $ element Shopify.appProvider
          { i18n: Shopify.enTranslations
          , children:
              [ element Shopify.emptyState
                  { heading: notNull "Session Error"
                  , action: null
                  , image: "https://cdn.shopify.com/s/files/1/0757/9955/files/empty-state.svg"
                  , children: [ R.text "Failed to detect a session, try reloading the app." ]
                  }
              ]
          }
