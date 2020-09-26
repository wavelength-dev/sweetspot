module Fulcrum.Service where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Argonaut (Json)
import Data.Argonaut (jsonParser, printJsonDecodeError, stringify) as Argonaut
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Effect.Aff (attempt) as Aff
import Foreign (readString) as Foreign
import Foreign.Index (readProp) as ForeignIndex
import Fulcrum.Config (apiUrl) as Config
import Fulcrum.Data (TestMap, decodeTestMaps)
import Fulcrum.User (UserId(..))
import Milkis (Fetch, Options, Response)
import Milkis as Milkis
import Milkis.Impl.Window (windowFetch) as MilkisImpl
import Record.Unsafe.Union (unsafeUnion) as RecordUnsafe

testMapEndpoint :: String
testMapEndpoint = Config.apiUrl <> "/bucket"

eventEndpoint :: String
eventEndpoint = Config.apiUrl <> "/event"

fetch :: Fetch
fetch = Milkis.fetch MilkisImpl.windowFetch

data TestMapProvisions
  = OnlyUserId UserId

getTestMapQueryString :: TestMapProvisions -> String
getTestMapQueryString = case _ of
  OnlyUserId (UserId uid) -> "?uid=" <> uid

fetchTestMaps :: TestMapProvisions -> Aff (Either String (Array TestMap))
fetchTestMaps provisions = do
  res <- getJson (testMapEndpoint <> qs) {}
  let
    status = Milkis.statusCode res
  bodyText <- Milkis.text res
  when (not (status == 200 || status == 201)) do
    throwError $ error $ show status <> " - " <> textToMessage bodyText
  pure (Argonaut.jsonParser bodyText >>= decodeTestMaps >>> lmap Argonaut.printJsonDecodeError)
  where
  qs = getTestMapQueryString provisions

  textToMessage "" = "Empty body"

  textToMessage str = "Body: " <> str

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

putJson :: String -> Json -> Aff Response
putJson url json =
  fetch
    (Milkis.URL url)
    { method: Milkis.putMethod
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
