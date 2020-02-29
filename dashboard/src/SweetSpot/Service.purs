module SweetSpot.Service where

import Prelude

import Data.Argonaut (decodeJson, jsonParser, Json)
import Data.Either (Either)
import Effect.Aff (Aff)
import Milkis (Options, Response, Fetch)
import Milkis as Milkis
import Milkis.Impl.Window as MilkisImpl
import Record.Unsafe.Union as RecordUnsafe
import SweetSpot.Data.Api (Product, UICampaign(..))
import SweetSpot.Data.Codec as Codec

fetch :: Fetch
fetch = Milkis.fetch MilkisImpl.windowFetch

getJson :: forall options. String -> Record options -> Aff Response
getJson url options = fetch (Milkis.URL url) combinedOptions
  where
  defaults =
    { method: Milkis.getMethod
    , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
    }

  combinedOptions :: Record Options
  combinedOptions = RecordUnsafe.unsafeUnion options defaults

shopParam :: String
shopParam = "?shop=libertyprice.myshopify.com"

fetchThing
  :: forall t.
     String
  -> (Json -> Either String t)
  -> Aff (Either String t)
fetchThing route decoder = do
  res <- getJson (route <> shopParam) {}
  text <- Milkis.text res
  pure (jsonParser text >>= decodeJson >>= decoder)

fetchCampaigns :: Aff (Either String (Array UICampaign))
fetchCampaigns =
  fetchThing "http://localhost:8082/api/dashboard/campaigns" Codec.decodeUICampaigns

fetchProducts :: Aff (Either String (Array Product))
fetchProducts =
  fetchThing "http://localhost:8082/api/dashboard/products" Codec.decodeProducts
