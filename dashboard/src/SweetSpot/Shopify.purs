module SweetSpot.Shopify where

import Prelude
import Data.JSDate (JSDate)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import React.Basic.Hooks (JSX, ReactComponent)

foreign import data I18N :: Type

foreign import enTranslations :: I18N

foreign import appProvider :: ReactComponent { i18n :: I18N, children :: Array JSX }

type Breadcrum
  = { content :: String, url :: String }

type Action = { content :: String, url :: String }
foreign import page ::
  ReactComponent
    { children :: Array JSX
    , subtitle :: Nullable String
    , title :: Nullable String
    , breadcrumbs :: Nullable (Array Breadcrum)
    , primaryAction :: Nullable (Action)
    }

foreign import card ::
  ReactComponent
    { title :: String
    , sectioned :: Boolean
    , children :: Array JSX
    }

foreign import button ::
  ReactComponent
    { url :: Nullable String
    , onClick :: Nullable (EffectFn1 Unit Unit)
    , primary :: Boolean
    , submit :: Boolean
    , children :: Array JSX
    }

foreign import emptyState ::
  ReactComponent
    { heading :: String
    , action :: { url :: String, content :: String }
    , image :: String
    , children :: Array JSX
    }

-- foreign import emptyState ::
--   ∀ props props_.
--   Union props props_ EmptyStateProps =>
--   ReactComponent (Record props)
foreign import resourceList :: ∀ a. ReactComponent { items :: Array a, renderItem :: a -> JSX }

type HeadingProps
  = ( element :: String, children :: Array JSX )

foreign import heading :: ReactComponent { element :: String, children :: JSX }

foreign import subheading :: ReactComponent { element :: String, children :: Array JSX }

foreign import textContainer :: ReactComponent { children :: Array JSX }

foreign import dataTable ::
  ReactComponent
    { columnContentTypes :: Array String
    , headings :: Array String
    , rows :: Array (Array String)
    }

foreign import form :: ReactComponent { onSubmit :: EffectFn1 Unit Unit, children :: Array JSX }

foreign import formLayout :: ReactComponent { children :: Array JSX }

foreign import textField ::
  ReactComponent
    { value :: String
    , onChange :: EffectFn1 String Unit
    , label :: String
    , labelHidden :: Boolean
    , children :: Array JSX
    }

foreign import datePicker ::
  ReactComponent
    { month :: Int
    , year :: Int
    , onChange :: EffectFn1 { start :: JSDate, end :: JSDate } Unit
    , onMonthChange :: EffectFn2 Int Int Unit
    , selected :: { start :: JSDate, end :: JSDate }
    , weekStartsOn :: Int
    }

foreign import modal ::
  ReactComponent
    { open :: Boolean
    , title :: String
    , onClose :: EffectFn1 Unit Unit
    , children :: Array JSX
    }

foreign import modalSection :: ReactComponent { key :: String, children :: Array JSX }

foreign import optionList ::
  ReactComponent
    { onChange :: EffectFn1 (Array String) Unit
    , options :: Array { value :: String, label :: String }
    , selected :: Array String
    , allowMultiple :: Boolean
    }
