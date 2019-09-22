module SweetSpot.Logging (class LogAction, log, buildLog, LogLevel(..)) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, (:=), (~>))
import Data.Argonaut (encodeJson, jsonEmptyObject, stringify) as Argonaut
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow) as Generic
import Effect (Effect)
import Effect.Aff (attempt, launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (error) as Console
import Effect.Exception (error)
import Milkis (statusCode) as Milkis
import SweetSpot.Api (sendLog) as Api

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
  log level message = Aff.launchAff_ do
      _response <- Aff.attempt $ Api.sendLog (buildLog level message)
      liftEffect $ case _response of
        Left logPostingErr -> Console.error $ format logPostingErr
        Right response ->
          if (Milkis.statusCode response) == 200 then
            pure unit
          else
            Console.error $ format logFailedErr
    where
    logFailedErr = error "Server responded with statusCode != 200 for log request"

    -- If we fail to communicate why we short-circuted to the server, we fallback to logging to console.
    format :: forall a. Show a => a -> String
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
