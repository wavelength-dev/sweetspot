module SweetSpot.Api where

import Prelude
import Data.Argonaut ((:=), (~>))
import Data.Argonaut as Ar
import Data.Either (Either(..))
import Effect.Aff (Aff, apathize, attempt)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import SweetSpot.Data.Codec (encodeViewEvent)
import SweetSpot.Data.Config (campaignIdQueryParam, eventEndpoint, experimentEndpoint, logEndpoint)
import SweetSpot.Data.Domain (CampaignId(..), TestMap, UserId(..), decodeTestMaps)
import SweetSpot.Data.Event (ViewEvent)

data TestMapProvisions
  = OnlyCampaignId CampaignId
  | OnlyUserId UserId
  | UserAndCampaignId UserId CampaignId

fetch :: M.Fetch
fetch = M.fetch windowFetch

fetchTestMaps :: TestMapProvisions -> Aff (Either String (Array TestMap))
fetchTestMaps provisions = do
  let
    opts =
      { method: M.getMethod
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      }

    qs = case provisions of
      UserAndCampaignId (UserId uid) (CampaignId cid) -> "?uid=" <> uid <> "&" <> campaignIdQueryParam <> "=" <> cid
      OnlyCampaignId (CampaignId cid) -> "?" <> campaignIdQueryParam <> "=" <> cid
      OnlyUserId (UserId uid) -> "?uid=" <> uid
  eResponseText <- attempt $ fetch (M.URL $ experimentEndpoint <> qs) opts >>= M.text
  pure
    $ case eResponseText of
        Left error -> show >>> Left $ error
        Right responseText -> Ar.jsonParser >=> decodeTestMaps $ responseText

postLogPayload :: forall a. Show a => a -> Aff M.Response
postLogPayload msg = fetch url opts
  where
  url = (M.URL logEndpoint)

  opts =
    { method: M.postMethod
    , headers: M.makeHeaders { "Content-Type": "application/json" }
    , body: Ar.stringify $ "message" := show msg ~> Ar.jsonEmptyObject
    }

postEventPayload :: ViewEvent -> Aff Unit
postEventPayload tv = apathize $ fetch url opts
  where
  url = (M.URL eventEndpoint)

  opts =
    { method: M.postMethod
    , headers: M.makeHeaders { "Content-Type": "application/json" }
    , body: Ar.stringify $ encodeViewEvent tv
    }
