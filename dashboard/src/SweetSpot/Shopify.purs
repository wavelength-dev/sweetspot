module SweetSpot.Shopify where

import Prelude
import Effect (Effect)
import Prim.Row (class Union)
import React.Basic.Hooks (JSX, ReactComponent, element)

type Action
  = { content :: String
    , onAction :: Effect Unit
    }

foreign import data I18N :: Type

foreign import enTranslations :: I18N

foreign import appProvider :: ReactComponent { i18n :: I18N, children :: JSX }

type Breadcrum
  = { content :: String, url :: String }

type PageProps
  = ( title :: String
    , subtitle :: String
    , children :: JSX
    , primaryAction :: Action
    , breadcrumbs :: Array Breadcrum
    )

foreign import page :: forall props props_. Union props props_ PageProps => ReactComponent (Record props)

foreign import card :: ReactComponent { title :: String, sectioned :: Boolean, children :: JSX }

type ButtonProps
  = ( url :: String, onClick :: Effect Unit, children :: JSX )

foreign import button :: forall props props_. Union props props_ ButtonProps => ReactComponent (Record props)

type EmptyStateProps
  = ( heading :: String
    , action :: Action
    , image :: String
    , children :: JSX
    )

foreign import emptyState :: ReactComponent (Record EmptyStateProps)

foreign import resourceList :: forall a. ReactComponent { items :: Array a, renderItem :: a -> JSX }

foreign import heading :: ReactComponent { element :: String, children :: JSX }

foreign import subheading :: ReactComponent { element :: String, children :: JSX }

