module SweetSpot.Request where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, apathize, attempt)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Codec (decodeUserBucket, encodeViewEvent)
import SweetSpot.Data.Constant (eventEndpoint, experimentEndpoint, logEndpoint)
import SweetSpot.Data.Event (ViewEvent)

fetch :: M.Fetch
fetch = M.fetch windowFetch

jsonHeader :: M.Headers
jsonHeader = M.makeHeaders { "Content-Type": "application/json" }

fetchUserBuckets :: Maybe String -> Maybe String -> Aff (Either String (Array UserBucket))
fetchUserBuckets mUid mCampaignId = do
  let
    opts =
      { method: M.getMethod
      , headers: jsonHeader
      }
    qs = case Tuple mUid mCampaignId of
      Tuple (Just uid) (Just campaignId) -> "?uid=" <> uid <> "&campaignId=" <> campaignId
      Tuple (Just uid) Nothing -> "?uid=" <> uid
      Tuple Nothing (Just campaignId) -> "?campaignId=" <> campaignId
      Tuple Nothing Nothing -> ""
  response <- attempt $ fetch (M.URL $ experimentEndpoint <> qs) opts
  case response of
    Right res -> M.text res >>= decodeUserBucket >>> pure
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
      , body:  stringify $ encodeViewEvent tv
      }
