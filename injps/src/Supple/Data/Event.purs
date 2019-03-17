module Supple.Data.Event where

import Prelude

import Data.Argonaut (class EncodeJson, (:=), (~>))
import Data.Maybe (Maybe)

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

newtype LineItem = LineItem
  { productId :: Number
  , variantId :: Number
  }

instance encodeLineItemJson :: EncodeJson LineItem where
  encodeJson (LineItem li) =
    "productId" := li.productId
    ~> "variantId" := li.variantId

type CheckoutEvent =
  { lineItems :: Maybe (Array LineItem)
  , step :: Maybe String
  , token :: Maybe String
  , page :: Page
  , pageUrl :: String
  , userId :: Maybe Number
  , orderId :: Maybe Number
  }
