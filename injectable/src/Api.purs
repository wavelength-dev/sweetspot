module SweetSpot.Api where

import Prelude
import Data.Argonaut (class EncodeJson, Json, (:=), (~>))
import Data.Argonaut (encodeJson, stringify) as Argonaut
import Data.Argonaut as Ar
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Milkis (Response)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import SweetSpot.Data.Config (campaignIdQueryParam, eventEndpoint, experimentEndpoint, logEndpoint)
import SweetSpot.Data.Domain (CampaignId(..), TestMap, UserId(..), decodeTestMaps)
import SweetSpot.Data.Event (ViewEvent)
import SweetSpot.Log (LogLevel)

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

jsonPost :: String -> Json -> Aff Response
jsonPost url json =
  fetch
    (M.URL url)
    { method: M.postMethod
    , headers: M.makeHeaders { "Content-Type": "application/json" }
    , body: Argonaut.stringify json
    }

postLogPayload :: forall a. EncodeJson a => LogLevel -> a -> Aff Response
postLogPayload level message = jsonPost logEndpoint json
  where
  json =
    "message" := message
      ~> "level"
      := level
      ~> Ar.jsonEmptyObject

postEventPayload :: ViewEvent -> Aff Response
postEventPayload viewEvent = jsonPost eventEndpoint (Argonaut.encodeJson viewEvent)
