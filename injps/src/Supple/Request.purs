module Supple.Request where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Effect.Aff (Aff, attempt)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Supple.Data.Api (UserBucket)
import Supple.Data.Constant (apiRoot)
import Supple.Data.Codec (decodeUserBucket)

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
  response <- attempt $ fetch (M.URL $ apiRoot <> "/bucket" <> qs) opts
  case response of
    Right res -> M.text res >>= decodeUserBucket >>> pure
    Left err -> pure $ Left $ show err

postLogPayload :: String -> Aff Unit
postLogPayload msg = do
  let
    opts =
      { method: M.postMethod
      , headers: jsonHeader
      , body: "{\"message\": \"" <> msg <> "\"}"
      }
  _ <- attempt $ fetch (M.URL $ apiRoot <> "/log") opts
  pure unit
