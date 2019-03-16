module Supple.Data.Event where

import Prelude

import Data.Maybe (Maybe)

data Page = Product | Collection | Collections | Checkout | Unknown

instance showPage :: Show Page where
  show page = case page of
    Product -> "product"
    Collection -> "collection"
    Collections -> "collections"
    Checkout -> "checkout"
    Unknown -> "unknown"

data Step = ThankYou | PaymentMethod

type ViewEvent =
  { expId :: Maybe Number
  , page :: Page
  , pageUrl :: String
  , userId :: Maybe String
  , productIds :: Maybe (Array Number)
  , productId :: Maybe Number
  }

type LineItem =
  { productId :: Number
  , variantId :: Number
  }

type CheckoutEvent =
  { page :: Page
  , token :: Maybe String
  , step :: Maybe Step
  , lineItems :: Maybe (Array LineItem)
  }
