module SweetSpot.Data.Event where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe)

data Page
  = Cart
  | Checkout
  | Collection
  | Collections
  | Home
  | Product
  | Unknown

derive instance eqPage :: Eq Page

instance showPage :: Show Page where
  show page = case page of
    Product -> "product"
    Cart -> "cart"
    Checkout -> "checkout"
    Collection -> "collection"
    Collections -> "collections"
    Home -> "home"
    Unknown -> "unknown"

instance encodeJsonPage :: EncodeJson Page where
  encodeJson = show >>> encodeJson

type ViewEvent
  = { page :: Page
    , pageUrl :: String
    , userId :: Maybe UserId
    , productIds :: Maybe (Array Number)
    , productId :: Maybe Number
    }

newtype LineItem
  = LineItem
  { productId :: Number
  , variantId :: Number
  , sku :: String
  , quantity :: Number
  }

instance encodeLineItemJson :: EncodeJson LineItem where
  encodeJson (LineItem li) =
    "productId" := li.productId
      ~> "variantId"
      := li.variantId
      ~> "sku"
      := li.sku
      ~> "quantity"
      := li.quantity
      ~> jsonEmptyObject

type CheckoutEvent
  = { lineItems :: Maybe (Array LineItem)
    , step :: Maybe String
    , token :: Maybe String
    , page :: Page
    , pageUrl :: String
    , userId :: Maybe Number
    , orderId :: Maybe Number
    }
