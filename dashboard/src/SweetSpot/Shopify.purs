module SweetSpot.Shopify where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import React.Basic (JSX, ReactComponent)

type Action
  = { content :: String
    , onAction :: Effect Unit
    }

foreign import data I18N :: Type

foreign import enTranslations :: I18N

foreign import appProvider :: ReactComponent { i18n :: I18N, children :: JSX }

foreign import page ::
  ReactComponent
    { title :: String
    , subtitle :: Nullable String
    , children :: Array JSX
    , primaryAction :: Nullable Action
    }

foreign import card :: ReactComponent { title :: String, sectioned :: Boolean, children :: Array JSX }

foreign import button :: ReactComponent { onClick :: Effect Unit, children :: String }

foreign import emptyState ::
  ReactComponent
    { heading :: Nullable String
    , action :: Nullable Action
    , image :: String
    , children :: Array JSX
    }

data Item a = Item a

foreign import resourceList :: forall a.  ReactComponent { items :: Array a, renderItem :: a -> JSX }

foreign import heading :: ReactComponent { element :: String, children :: String}

foreign import subheading :: ReactComponent { element :: String, children :: String}
