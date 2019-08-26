module SweetSpot.Data.Codec where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Traversable (sequence)
import SweetSpot.Data.Event (CheckoutEvent, ViewEvent)
import SweetSpot.Data.Shopify (Product, Variant)

decodeVariant :: Json -> Either String Variant
decodeVariant json = do
  o <- decodeJson json
  id <- o .: "id"
  sku <- o .: "sku"
  price <- o .: "price"
  pure { id, sku, price }

decodeProduct :: String -> Either String Product
decodeProduct str = do
  json <- jsonParser str
  o <- decodeJson json
  id <- o .: "id"
  variants <- sequence <<< map decodeVariant =<< o .: "variants"
  pure { id, variants }

encodeViewEvent :: ViewEvent -> Json
encodeViewEvent { page, pageUrl, userId, productId, productIds } =
  "page" := (show page)
  ~> "pageUrl" := pageUrl
  ~> "userId" := userId
  ~> "productId" := productId
  ~> "productIds" := productIds
  ~> jsonEmptyObject

encodeCheckoutEvent :: CheckoutEvent -> Json
encodeCheckoutEvent { page, pageUrl, step, token, orderId, lineItems, userId } =
  "page" := (show page)
  ~> "pageUrl" := pageUrl
  ~> "step" := step
  ~> "token" := token
  ~> "orderId" := orderId
  ~> "lineItems" := lineItems
  ~> "userId" := userId
  ~> jsonEmptyObject
