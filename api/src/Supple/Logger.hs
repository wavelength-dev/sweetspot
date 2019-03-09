{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Supple.Logger
  ( info
  , warn
  , error
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
  ( ToJSON(..)
  , defaultOptions
  , encode
  , genericToEncoding
  , toEncoding
  )
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Prelude hiding (log, error)
import Supple.AppM (AppCtx(..), AppM)
import System.Log.FastLogger (ToLogStr(..), pushLogStrLn)

data LogLevel
  = Info
  | Warn
  | Error
  deriving (Eq, Show)

instance ToJSON LogLevel where
  toJSON lvl = case lvl of
    Info -> "info"
    Warn -> "warn"
    Error -> "error"

data LogMessage = LogMessage
  { level :: !LogLevel
  , message :: !Text
  , timestamp :: !UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

log :: LogLevel -> Text -> AppM ()
log lvl msg = do
  logset <- asks _getLogger
  ts <- liftIO getCurrentTime
  liftIO $
    pushLogStrLn logset $
    toLogStr LogMessage {level = lvl, message = msg, timestamp = ts}

info :: Text -> AppM ()
info msg = log Info msg

warn :: Text -> AppM ()
warn msg = log Warn msg

error :: Text -> AppM ()
error msg = log Error msg
