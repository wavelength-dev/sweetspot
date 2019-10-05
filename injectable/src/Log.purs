module SweetSpot.Log (class LogAction, log, buildLog, LogLevel(..)) where

import Prelude
import Data.Argonaut (class EncodeJson, Json, (:=), (~>))
import Data.Argonaut (encodeJson, jsonEmptyObject, stringify) as Argonaut
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow) as Generic
import Effect (Effect)
import Effect.Aff (attempt, launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (error, info, warn) as Console
import Effect.Exception (error)
import Milkis (statusCode) as Milkis
import SweetSpot.Api (sendLog) as Api
import SweetSpot.Debug (getDebugMode) as Debug

data LogLevel
  = Info
  | Warn
  | Error

derive instance genericLogLevel :: Generic LogLevel _

instance showLogLevel :: Show LogLevel where
  show = Generic.genericShow

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson Info = Argonaut.encodeJson "info"
  encodeJson Warn = Argonaut.encodeJson "warn"
  encodeJson Error = Argonaut.encodeJson "error"

class
  Monad m <= LogAction m where
  log :: forall a. EncodeJson a => LogLevel -> a -> m Unit

instance monadLogEffect :: LogAction Effect where
  log = log_

log_ :: forall a. EncodeJson a => LogLevel -> a -> Effect Unit
log_ level message =
  Aff.launchAff_ do
    _response <- Aff.attempt $ Api.sendLog (buildLog level message)
    liftEffect
      $ case _response of
          Left logPostingErr -> Console.error $ format logPostingErr
          Right response -> do
            debugMode <- Debug.getDebugMode
            if (Milkis.statusCode response) == 200 then
              if debugMode == true then case level of
                Info -> message # argonautShow >>> Console.info
                Warn -> message # argonautShow >>> Console.warn
                Error -> message # argonautShow >>> Console.error
              else
                pure unit
            else
              Console.error $ format logFailedErr
  where
  argonautShow = Argonaut.encodeJson >>> Argonaut.stringify

  logFailedErr = error "Server responded with statusCode != 200 for log request"

  -- If we fail to communicate why we short-circuted to the server, we fallback to logging to console.
  format :: forall b. Show b => b -> String
  format logErr =
    "Failed to post log message, falling back to console.\n"
      <> "Logging error: "
      <> show logErr
      <> "App log, level: "
      <> show level
      <> ", message: "
      <> (message # Argonaut.encodeJson >>> Argonaut.stringify)

buildLog :: forall a. EncodeJson a => LogLevel -> a -> Json
buildLog level message =
  "message" := message
    ~> "level"
    := level
    ~> Argonaut.jsonEmptyObject
