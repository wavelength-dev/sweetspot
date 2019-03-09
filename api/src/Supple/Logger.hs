{-# LANGUAGE OverloadedStrings #-}

module Supple.Logger where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time.Clock (getCurrentTime)
import Supple.AppM (AppM, LogMessage(..), AppCtx(..))
import System.Log.FastLogger (ToLogStr(..), pushLogStrLn)

log :: Text -> AppM ()
log msg = do
  logset <- asks _getLogger
  ts <- liftIO getCurrentTime
  liftIO $ pushLogStrLn logset $ toLogStr LogMessage {message = msg, timestamp = ts}
