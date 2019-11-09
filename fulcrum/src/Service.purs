module Sprice.Service where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut (encodeJson, jsonParser, stringify) as Argonaut
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Foreign (readString) as Foreign
import Foreign.Index (readProp) as ForeignIndex
import Milkis (Fetch, Options, Response)
import Milkis (URL(..), fetch, getMethod, json, makeHeaders, postMethod, statusCode, text) as Milkis
import Milkis.Impl.Window (windowFetch) as MilkisImpl
import Record.Unsafe.Union (unsafeUnion) as RecordUnsafe
import Sprice.Config (apiUrl) as Config
import Sprice.Data (CampaignId(..), TestMap, decodeTestMaps)
import Sprice.User (UserId(..))

testMapEndpoint :: String
testMapEndpoint = Config.apiUrl <> "/bucket"

eventEndpoint :: String
eventEndpoint = Config.apiUrl <> "/event"

logEndpoint :: String
logEndpoint = Config.apiUrl <> "/log"

fetch :: Fetch
fetch = Milkis.fetch MilkisImpl.windowFetch

data TestMapProvisions
  = OnlyCampaignId CampaignId
  | OnlyUserId UserId
  | UserAndCampaignId UserId CampaignId

getTestMapQueryString :: TestMapProvisions -> String
getTestMapQueryString = case _ of
  UserAndCampaignId (UserId uid) (CampaignId cid) -> "?uid=" <> uid <> "&sscid=" <> cid
  OnlyCampaignId (CampaignId cid) -> "?sscid=" <> cid
  OnlyUserId (UserId uid) -> "?uid=" <> uid

fetchTestMaps :: TestMapProvisions -> Aff (Either String (Array TestMap))
fetchTestMaps provisions = do
  res <- getJson (testMapEndpoint <> qs) {}
  text <- Milkis.text res
  pure (Argonaut.jsonParser text >>= decodeTestMaps)
  where
  qs = getTestMapQueryString provisions

getJson :: forall options. String -> Record options -> Aff Response
getJson url options = fetch (Milkis.URL url) combinedOptions
  where
  defaults =
    { method: Milkis.getMethod
    , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
    }

  combinedOptions :: Record Options
  combinedOptions = RecordUnsafe.unsafeUnion options defaults

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
