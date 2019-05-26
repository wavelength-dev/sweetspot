module SweetSpot.Data.Codec where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Traversable (sequence)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Event (CheckoutEvent, ViewEvent)
import SweetSpot.Data.Shopify (Product, Variant)


-- | ---------------------------------------------------------------------------
-- | Decode
-- | ---------------------------------------------------------------------------
decodeUserBucket :: String -> Either String UserBucket
decodeUserBucket str = do
  json <- jsonParser str
  o <- decodeJson json
  _ubUserId <- o .: "_ubUserId"
  _ubSku  <- o .: "_ubSku"
  _ubSvid  <- o .: "_ubSvid"
  _ubPrice  <- o .: "_ubPrice"
  _ubExpId  <- o .: "_ubExpId"
  _ubBucketId  <- o .: "_ubBucketId"
  pure $ UserBucket
   { _ubUserId , _ubSku , _ubSvid , _ubPrice , _ubExpId , _ubBucketId}

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
  ~> "expId" := ve.expId
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
