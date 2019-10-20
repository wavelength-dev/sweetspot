module Sprice.Service where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut (encodeJson, stringify) as Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Foreign (readString) as Foreign
import Foreign.Index (readProp) as ForeignIndex
import Milkis (Response, Fetch)
import Milkis (URL(..), fetch, json, makeHeaders, postMethod, statusCode) as Milkis
import Milkis.Impl.Window (windowFetch) as MilkisImpl
import Sprice.Config (apiUrl) as Config

eventEndpoint :: String
eventEndpoint = Config.apiUrl <> "/event"

logEndpoint :: String
logEndpoint = Config.apiUrl <> "/log"

fetch :: Fetch
fetch = Milkis.fetch MilkisImpl.windowFetch

postJson :: String -> Json -> Aff Response
postJson url json =
  fetch
    (Milkis.URL url)
    { method: Milkis.postMethod
    , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
    , body: Argonaut.stringify json
    }

getServiceError :: Response -> Aff (Maybe String)
getServiceError response = do
  eFBody <- Aff.attempt $ Milkis.json response
  let
    mMessage = case eFBody of
      -- failed to build json from response
      Left err -> Nothing
      Right fBody -> case runExcept (ForeignIndex.readProp "message" fBody >>= Foreign.readString) of
        -- failed to read message prop from json response
        Left _ -> Nothing
        Right message -> Just message
  pure mMessage

sendLog :: forall a. EncodeJson a => a -> Aff (Either String Unit)
sendLog log = do
  let
    json = Argonaut.encodeJson log
  response <- postJson logEndpoint json
  mMessage <- getServiceError response
  let
    status = Milkis.statusCode response
  pure
    if status == 200 then
      Right unit
    else case mMessage of
      Nothing -> Left $ "sending log failed, status: " <> show status <> ", no body"
      Just message -> Left $ "sending log failed, status: " <> show status <> ", body: " <> message
