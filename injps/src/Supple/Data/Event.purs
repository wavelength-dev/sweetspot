module Supple.Data.Event where

import Prelude

import Data.Maybe (Maybe)
import Foreign (Foreign)

data Page = Product | Collection | Collections | Checkout | Unknown

instance showPage :: Show Page where
  show page = case page of
    Product -> "product"
    Collection -> "collection"
    Collections -> "collections"
    Checkout -> "checkout"
    Unknown -> "unknown"

type ViewEvent =
  { expId :: Number
  , bucketId :: Number
  , page :: Page
  , pageUrl :: String
  , userId :: Maybe String
  , productIds :: Maybe (Array Number)
  , productId :: Maybe Number
  }

type LineItem = Foreign
  -- { productId :: Number
  -- , variantId :: Number
  -- }

type CheckoutEvent =
  { lineItems :: Maybe (Array LineItem)
  , step :: Maybe String
  , token :: Maybe String
  , page :: Page
  , pageUrl :: String
  , orderId :: Maybe Number
  }
