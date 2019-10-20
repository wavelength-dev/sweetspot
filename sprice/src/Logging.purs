module Sprice.Logging (log, buildLog, LogLevel(..)) where

import Prelude
import Data.Argonaut (class EncodeJson, Json, (:=), (~>))
import Data.Argonaut (encodeJson, jsonEmptyObject, stringify) as Argonaut
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Console (error, info, warn) as Console
import Sprice.Service (sendLog) as Service
import Sprice.Site (getUrlParam) as Site

data LogLevel
  = Info
  | Warn
  | Error

instance showLogLevel :: Show LogLevel where
  show Info = "info"
  show Warn = "warn"
  show Error = "error"

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson Info = Argonaut.encodeJson "info"
  encodeJson Warn = Argonaut.encodeJson "warn"
  encodeJson Error = Argonaut.encodeJson "error"

log :: forall a. EncodeJson a => LogLevel -> a -> Effect Unit
log level message = do
  isDebugging <- getIsDebugging
  when isDebugging case level of
    Info -> message # argonautShow >>> Console.info
    Warn -> message # argonautShow >>> Console.warn
    Error -> message # argonautShow >>> Console.error
  Aff.launchAff_ do
    eSendLogResult <- Service.sendLog (buildLog level message)
    case eSendLogResult of
      Right _ -> pure unit
      Left sendLogError ->
        liftEffect do
          Console.info $ "failed to send log message, falling back to console."
          Console.error $ "send log error: " <> sendLogError
          Console.error $ "app log, level: "
            <> show level
            <> ", message: "
            <> argonautShow message
  where
  argonautShow = Argonaut.encodeJson >>> Argonaut.stringify

buildLog :: forall a. EncodeJson a => LogLevel -> a -> Json
buildLog level message =
  "message" := message
    ~> "level"
    := level
    ~> Argonaut.jsonEmptyObject

getIsDebugging :: Effect Boolean
getIsDebugging = Site.getUrlParam "ssdebug" >>= maybe false ((==) "true") >>> pure
