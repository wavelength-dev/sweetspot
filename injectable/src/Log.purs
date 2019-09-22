module SweetSpot.Log (class MonadLog, log, LogLevel(..)) where

import Data.Argonaut (class EncodeJson)
import Data.Argonaut (encodeJson) as Argonaut
import Prelude (class Monad, class Show, Unit)

data LogLevel = Info | Warn | Error

instance encodeJsonLogLevel :: EncodeJson LogLevel where
  encodeJson Info = Argonaut.encodeJson "info"
  encodeJson Warn = Argonaut.encodeJson "warn"
  encodeJson Error = Argonaut.encodeJson "error"

class Monad m <= MonadLog m where
  log :: forall a. Show a => a -> m Unit
