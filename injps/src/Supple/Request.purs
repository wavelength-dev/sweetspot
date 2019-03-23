module Supple.Request where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect.Aff (Aff, apathize, attempt)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Supple.Data.Api (UserBucket)
import Supple.Data.Codec (decodeUserBucket, encodeViewEvent)
import Supple.Data.Constant (eventEndpoint, experimentEndpoint, logEndpoint)
import Supple.Data.Event (ViewEvent)

fetch :: M.Fetch
fetch = M.fetch windowFetch

jsonHeader :: M.Headers
jsonHeader = M.makeHeaders { "Content-Type": "application/json" }

fetchUserBuckets :: Maybe String -> Aff (Either String UserBucket)
fetchUserBuckets uid = do
  let
    opts =
      { method: M.getMethod
      , headers: jsonHeader
      }
    qs = maybe "" ((<>) "?uid=") uid
  response <- attempt $ fetch (M.URL $ experimentEndpoint <> qs) opts
  case response of
    Right res -> M.text res >>= decodeUserBucket >>> pure
    Left err -> pure $ Left $ show err

postLogPayload :: String -> Aff Unit
postLogPayload msg = apathize $ fetch url opts
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
