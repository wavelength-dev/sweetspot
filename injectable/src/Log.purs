module SweetSpot.Log (LogLevel(..)) where

import Data.Argonaut (class EncodeJson)
import Data.Argonaut (encodeJson) as Argonaut

data LogLevel = Info | Warn | Error

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson Info = Argonaut.encodeJson "info"
  encodeJson Warn = Argonaut.encodeJson "warn"
  encodeJson Error = Argonaut.encodeJson "error"
