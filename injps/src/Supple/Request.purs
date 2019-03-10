module Supple.Request where

import Prelude
import Supple.Data.Api (UserBucket)
import Supple.Data.Codec (decodeResponse)

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff, attempt)
import Milkis as M
import Milkis.Impl.Window (windowFetch)

fetch :: M.Fetch
fetch = M.fetch windowFetch

fetchUserBuckets :: Maybe String -> Aff (Maybe (Array UserBucket))
fetchUserBuckets uid = do
  let
    opts =
      { method: M.getMethod
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      }
    qs = maybe "" ((<>) "?uid=") uid
  result <- attempt $ fetch (M.URL $ "https://b62c97ea.ngrok.io/api/bucket" <> qs) opts
  case result of
    Right res -> do
      s <- M.text res
      pure $ hush $ decodeResponse s
    Left _ -> pure Nothing
