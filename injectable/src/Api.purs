module SweetSpot.Api where

import Prelude
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut (encodeJson, stringify) as Argonaut
import Data.Argonaut as Ar
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Milkis (Response)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import SweetSpot.Data.Config (eventEndpoint, experimentEndpoint, logEndpoint)
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
      UserAndCampaignId (UserId uid) (CampaignId cid) -> "?uid=" <> uid <> "&sscid=" <> cid
      OnlyCampaignId (CampaignId cid) -> "?sscid=" <> cid
      OnlyUserId (UserId uid) -> "?uid=" <> uid
  eResponseText <- attempt $ fetch (M.URL $ experimentEndpoint <> qs) opts >>= M.text
  pure
    $ case eResponseText of
        Left error -> show >>> Left $ error
        Right responseText -> Ar.jsonParser >=> decodeTestMaps $ responseText

postJson :: String -> Json -> Aff Response
postJson url json =
  fetch
    (M.URL url)
    { method: M.postMethod
    , headers: M.makeHeaders { "Content-Type": "application/json" }
    , body: Argonaut.stringify json
    }

sendLog :: forall a. EncodeJson a => a -> Aff Response
sendLog = Argonaut.encodeJson >>> postJson logEndpoint

sendEvent :: ViewEvent -> Aff Response
sendEvent = Argonaut.encodeJson >>> postJson eventEndpoint
