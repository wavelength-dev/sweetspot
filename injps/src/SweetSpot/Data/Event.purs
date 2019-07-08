module SweetSpot.Data.Event where

import Prelude

import Data.Argonaut (class EncodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe)

data Page = Product | Collection | Collections | Checkout | Home | Unknown

instance showPage :: Show Page where
  show page = case page of
    Product -> "product"
    Collection -> "collection"
    Collections -> "collections"
    Checkout -> "checkout"
    Home -> "home"
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
  , sku :: String
  , quantity :: Number
  }

instance encodeLineItemJson :: EncodeJson LineItem where
  encodeJson (LineItem li) =
    "productId" := li.productId
    ~> "variantId" := li.variantId
    ~> "sku" := li.sku
    ~> "quantity" := li.quantity
    ~> jsonEmptyObject

type CheckoutEvent =
  { lineItems :: Maybe (Array LineItem)
  , step :: Maybe String
  , token :: Maybe String
  , page :: Page
  , pageUrl :: String
  , userId :: Maybe Number
  , orderId :: Maybe Number
  }
