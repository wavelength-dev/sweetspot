module SweetSpot.Service where

import Prelude

import Data.Argonaut (Json, jsonParser)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Effect.Exception.Unsafe (unsafeThrow) as Unsafe
import Milkis (Options, Response, Fetch)
import Milkis as Milkis
import Milkis.Impl.Window as MilkisImpl
import Record.Unsafe.Union as RecordUnsafe
import SweetSpot.Data.Api (Product, UICampaign)
import SweetSpot.Data.Codec (decodeProducts, decodeUICampaigns) as Codec
import SweetSpot.QueryString (buildQueryString) as QueryString
import SweetSpot.Session (SessionId(..))

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

serviceUrl :: String
serviceUrl = "//localhost:8082/api/dashboard/"

data Resource
  = Campaigns
  | Products

getResourceRoute :: Resource -> String
getResourceRoute Campaigns = serviceUrl <> "campaigns"

getResourceRoute Products = serviceUrl <> "products"

fetchResource :: Resource -> SessionId -> Aff (Either String Json)
fetchResource resource (SessionId sessionId) =
  Aff.attempt (getJson (route <> queryString) {})
    >>= case _ of
        Left requestErrMsg -> requestErrMsg # show >>> Left >>> pure
        Right res -> do
          bodyText <- Milkis.text res
          pure
            if status == 200 || status == 201 then
              jsonParser bodyText
            else
              Left (show status <> " - " <> textToMessage bodyText)
          where
          status = Milkis.statusCode res

          textToMessage "" = "Empty body"

          textToMessage str = "Body: " <> str
  where
  route = getResourceRoute resource

  queryString =
    QueryString.buildQueryString [ Tuple "session" sessionId ]
      # case _ of
          Left errMsg -> Unsafe.unsafeThrow errMsg
          Right qs -> qs

fetchCampaigns :: SessionId -> Aff (Either String (Array UICampaign))
fetchCampaigns sessionId = fetchResource Campaigns sessionId <#> \eCampaigns -> eCampaigns >>= Codec.decodeUICampaigns

fetchProducts :: SessionId -> Aff (Either String (Array Product))
fetchProducts sessionId = fetchResource Products sessionId <#> \eProducts -> eProducts >>= Codec.decodeProducts
