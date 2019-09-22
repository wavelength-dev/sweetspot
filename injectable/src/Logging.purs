module SweetSpot.Logging (class LogAction, log, LogLevel(..)) where

import Prelude

import Data.Argonaut (class EncodeJson, Json, (:=), (~>))
import Data.Argonaut (encodeJson, jsonEmptyObject) as Argonaut
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow) as Generic
import Effect (Effect)
import Effect.Aff (runAff_) as Aff
import Effect.Console (error) as Console
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

class Monad m <= LogAction m where
  log :: forall a. Show a => LogLevel -> a -> m Unit

instance monadLogEffect :: LogAction Effect where
  log level message = Aff.runAff_ onLogResult $ Api.sendLog (buildLog level message)
    where
    -- If we fail to communicate why we short-circuted to the server, we fallback to logging to console.
    onLogResult result = case result of
      Right _ -> pure unit
      Left logPostingErr -> Console.error $ format logPostingErr

    format logErr =
      "Failed to post log message, falling back to console.\n"
        <> "Logging error: "
        <> show logErr
        <> "App log, level: "
        <> show level
        <> ", message: "
        <> show message

buildLog :: forall a. Show a => LogLevel -> a -> Json
buildLog level message =
  "message" := show message
    ~> "level"
    := level
    ~> Argonaut.jsonEmptyObject
