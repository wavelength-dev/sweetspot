module SweetSpot.Data.Codec where

import Prelude
import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Record (delete)
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
encodeViewEvent viewEvent@{ page } =
  let
    simpleRows = delete (SProxy :: SProxy "page") viewEvent
  in
    "page" := (show page) ~> encodeJson simpleRows

encodeCheckoutEvent :: CheckoutEvent -> Json
encodeCheckoutEvent checkoutEvent@{ page } =
  let
    simpleRows = delete (SProxy :: SProxy "page") checkoutEvent
  in
    "page" := (show page) ~> encodeJson simpleRows
