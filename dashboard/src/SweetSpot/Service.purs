module SweetSpot.Service (fetchCampaigns, fetchProducts) where

import Prelude
import SweetSpot.Data.Api (Product, UICampaign)

import Data.Argonaut (class DecodeJson, decodeJson, jsonParser)
import Data.Either (Either)
import Effect.Aff (Aff)
import Milkis (Options, Response, Fetch)
import Milkis as Milkis
import Milkis.Impl.Window as MilkisImpl
import Record.Unsafe.Union as RecordUnsafe

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

fetchThing :: forall t. (DecodeJson t) => String -> Aff (Either String (Array t))
fetchThing route = do
  res <- getJson (route <> shopParam) {}
  text <- Milkis.text res
  pure (jsonParser text >>= decodeJson)

fetchCampaigns :: Aff (Either String (Array UICampaign))
fetchCampaigns = fetchThing "http://localhost:8082/api/dashboard/campaigns"

fetchProducts :: Aff (Either String (Array Product))
fetchProducts = fetchThing "http://localhost:8082/api/dashboard/products"
