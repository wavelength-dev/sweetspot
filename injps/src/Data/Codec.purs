module SweetSpot.Data.Codec where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Traversable (for, sequence)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Event (CheckoutEvent, ViewEvent)
import SweetSpot.Data.Shopify (Product, Variant)


-- | ---------------------------------------------------------------------------
-- | Decode
-- | ---------------------------------------------------------------------------
decodeUserBucket :: Json -> Either String UserBucket
decodeUserBucket json = do
  x <- decodeJson json
  _ubUserId <- x .: "_ubUserId"
  _ubSku <- x .: "_ubSku"
  _ubOriginalSvid <- x .: "_ubOriginalSvid"
  _ubTestSvid <- x .: "_ubTestSvid"
  _ubPrice <- x .: "_ubPrice"
  _ubExpId <- x .: "_ubExpId"
  _ubBucketId  <- x .: "_ubBucketId"
  _ubBucketType <- x .: "_ubBucketType"
  pure { _ubUserId
       , _ubSku
       , _ubTestSvid
       , _ubPrice
       , _ubExpId
       , _ubBucketId
       , _ubOriginalSvid
       , _ubBucketType
       }

decodeUserBuckets :: String -> Either String (Array UserBucket)
decodeUserBuckets str = do
  json <- jsonParser str
  x <- decodeJson json
  for x decodeUserBucket

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


-- | ---------------------------------------------------------------------------
-- | Encode
-- | ---------------------------------------------------------------------------
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
