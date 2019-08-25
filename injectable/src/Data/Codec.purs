module SweetSpot.Data.Codec where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Traversable (sequence, traverse)
import SweetSpot.Data.Api (TestMap)
import SweetSpot.Data.Event (CheckoutEvent, ViewEvent)
import SweetSpot.Data.Shopify (Product, Variant)

decodeTestMaps :: Json -> Either String (Array TestMap)
decodeTestMaps json = decodeJson json >>= traverse decodeJson

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
encodeViewEvent ve =
  "page" := (show ve.page)
  ~> "pageUrl" := ve.pageUrl
  ~> "userId" := ve.userId
  ~> "productId" := ve.productId
  ~> "productIds" := ve.productIds
  ~> jsonEmptyObject

encodeCheckoutEvent :: CheckoutEvent -> Json
encodeCheckoutEvent ve =
  "page" := (show ve.page)
  ~> "pageUrl" := ve.pageUrl
  ~> "step" := ve.step
  ~> "token" := ve.token
  ~> "orderId" := ve.orderId
  ~> "lineItems" := ve.lineItems
  ~> "userId" := ve.userId
  ~> jsonEmptyObject
