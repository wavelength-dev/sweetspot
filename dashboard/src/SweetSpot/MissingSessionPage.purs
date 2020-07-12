module SweetSpot.MissingSessionPage where

import Data.Nullable (null)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (appProvider, emptyState, enTranslations) as Shopify

missingSessionPage :: JSX
missingSessionPage =
  element Shopify.appProvider
    { i18n: Shopify.enTranslations
    , children:
        [ element Shopify.emptyState
            { heading: "Session Error"
            , image: "https://cdn.shopify.com/s/files/1/0757/9955/files/empty-state.svg"
            , children: [ R.text "Failed to detect a session, try reloading the app." ]
            , action: null
            }
        ]
    }
