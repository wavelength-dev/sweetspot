module Shopify where

import Prelude
import Effect (Effect)
import React.Basic (JSX, ReactComponent)

foreign import data I18N :: Type

foreign import enTranslations :: I18N

foreign import appProvider :: ReactComponent { i18n :: I18N, children :: Array JSX }

foreign import page :: ReactComponent { title :: String, children :: Array JSX }

foreign import card :: ReactComponent { sectioned :: Boolean, children :: Array JSX }

foreign import button :: ReactComponent { onClick :: Effect Unit, children :: Array JSX }
