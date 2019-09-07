module SweetSpot.Data.Codec where

import Prelude

import Data.Argonaut (encodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Record (delete)
import SweetSpot.Data.Event (CheckoutEvent, ViewEvent)
import SweetSpot.Data.Shopify (Product, Variant)

-- TODO: use an instance here
decodeVariant :: Json -> Either String Variant
decodeVariant json = do
  obj <- decodeJson json
  id <- obj .: "id"
  sku <- obj .: "sku"
  price <- obj .: "price"
  pure { id, sku, price }

-- TODO: use an instance here
decodeProduct :: String -> Either String Product
decodeProduct rawJson = do
  json <- jsonParser rawJson
  obj <- decodeJson json
  id <- obj .: "id"
  variants <- obj .: "variants" >>= traverse decodeVariant
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
