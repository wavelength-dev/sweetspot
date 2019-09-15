{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Logger
  ( info
  , info'
  , warn
  , error
  , error'
  ) where

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class     ( MonadReader(..) )
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
import Prelude hiding (error, log)
import SweetSpot.AppM (AppCtx(..))
import System.Log.FastLogger (LoggerSet, ToLogStr(..), pushLogStrLn)

data LogLevel
  = Info
  | Warn
  | Error
  deriving (Eq, Show)

instance ToJSON LogLevel where
  toJSON lvl =
    case lvl of
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

log :: (MonadReader AppCtx m, MonadIO m) => LogLevel -> Text -> m ()
log lvl msg = do
  logset <- asks _getLogger
  ts <- liftIO getCurrentTime
  liftIO $
    pushLogStrLn logset $
    toLogStr LogMessage {level = lvl, message = msg, timestamp = ts}

info :: (MonadReader AppCtx m, MonadIO m) => Text -> m ()
info = log Info

warn :: (MonadReader AppCtx m, MonadIO m) => Text -> m ()
warn = log Warn

error :: (MonadReader AppCtx m, MonadIO m) => Text -> m ()
error = log Error

log' :: LogLevel -> LoggerSet -> Text -> IO ()
log' lvl logset msg = do
  ts <- getCurrentTime
  pushLogStrLn logset $
    toLogStr LogMessage {level = lvl, message = msg, timestamp = ts}

info' :: LoggerSet -> Text -> IO ()
info' = log' Info

error' :: LoggerSet -> Text -> IO ()
error' = log' Error
