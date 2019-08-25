module SweetSpot.Api where

import Prelude

import Data.Argonaut (fromString) as Ar
import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Effect.Aff (Aff, apathize, attempt)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import SweetSpot.Data.Api (TestMap, decodeTestMaps)
import SweetSpot.Data.Codec (encodeViewEvent)
import SweetSpot.Data.Config (campaignIdQueryParam, eventEndpoint, experimentEndpoint, logEndpoint)
import SweetSpot.Data.Domain (CampaignId(..), UserId(..))
import SweetSpot.Data.Event (ViewEvent)

data TestMapProvisions
  = OnlyCampaignId CampaignId
  | OnlyUserId UserId
  | UserAndCampaignId UserId CampaignId

fetch :: M.Fetch
fetch = M.fetch windowFetch

jsonHeader :: M.Headers
jsonHeader = M.makeHeaders { "Content-Type": "application/json" }

fetchTestMaps :: TestMapProvisions -> Aff (Either String (Array TestMap))
fetchTestMaps provisions = do
  let
    opts =
      { method: M.getMethod
      , headers: jsonHeader
      }

    qs = case provisions of
      UserAndCampaignId (UserId uid) (CampaignId cid) -> "?uid=" <> uid <> "&" <> campaignIdQueryParam <> "=" <> cid
      OnlyCampaignId (CampaignId cid) -> "?" <> campaignIdQueryParam <> "=" <> cid
      OnlyUserId (UserId uid) -> "?uid=" <> uid
  response <- attempt $ fetch (M.URL $ experimentEndpoint <> qs) opts
  case response of
    Right res -> M.text res >>= Ar.fromString >>> decodeTestMaps >>> pure
    Left err -> pure $ Left $ show err

postLogPayload :: String -> Aff M.Response
postLogPayload msg = fetch url opts
  where
  url = (M.URL logEndpoint)

  opts =
    { method: M.postMethod
    , headers: jsonHeader
    , body: "{\"message\": \"" <> msg <> "\"}"
    }

postEventPayload :: ViewEvent -> Aff Unit
postEventPayload tv = apathize $ fetch url opts
  where
  url = (M.URL eventEndpoint)

  opts =
    { method: M.postMethod
    , headers: jsonHeader
    , body: stringify $ encodeViewEvent tv
    }
