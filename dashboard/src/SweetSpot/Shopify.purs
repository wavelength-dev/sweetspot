module SweetSpot.Shopify where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import React.Basic.Hooks (JSX, ReactComponent, element)

type Action
  = { content :: String
    , onAction :: Nullable (Effect Unit)
    , url :: Nullable String
    }

foreign import data I18N :: Type

foreign import enTranslations :: I18N

foreign import appProvider :: ReactComponent { i18n :: I18N, children :: JSX }

type Breadcrum
  = { content :: String, url :: String }

foreign import page ::
  forall a. ReactComponent (Record a)

-- { title :: String
-- , subtitle :: Nullable String
-- , children :: JSX
-- , primaryAction :: Nullable Action
-- , breadcrumbs :: Array Breadcrum
-- }
foreign import card ::
  ReactComponent
    { title :: String
    , sectioned :: Boolean
    , children :: JSX
    }

foreign import button ::
  ReactComponent
    { url :: Nullable String
    , onClick :: Nullable (Effect Unit)
    , children :: JSX
    }

foreign import emptyState ::
  ReactComponent
    { heading :: String
    , action :: Nullable Action
    , image :: String
    , children :: Array JSX
    }

foreign import resourceList :: forall a. ReactComponent { items :: Array a, renderItem :: a -> JSX }

foreign import heading :: ReactComponent { element :: String, children :: JSX }

foreign import subheading :: ReactComponent { element :: String, children :: JSX }

foreign import textContainer :: ReactComponent { children :: JSX }

textContainer_ :: JSX -> JSX
textContainer_ children = element textContainer { children }

foreign import dataTable ::
  ReactComponent
    { columnContentTypes :: Array String
    , headings :: Array String
    , rows :: Array (Array String)
    }
