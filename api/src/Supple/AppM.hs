{-# LANGUAGE DeriveGeneric #-}

module Supple.AppM where

import Control.Monad.Reader (ReaderT)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (Handler)
import Supple.Database (Connection)
import System.Log.FastLogger (LoggerSet)

data AppConfig = AppConfig
  { environment :: !Text
  , version :: !Text
  } deriving (Generic, Show)

data AppCtx = AppCtx
  { _getConfig :: !AppConfig
  , _getLogger :: !LoggerSet
  , _getDbConn :: !Connection
  }

type AppM = ReaderT AppCtx Handler
