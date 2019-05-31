{-# LANGUAGE DeriveGeneric #-}

module Supple.AppM where

import Control.Monad.Reader (ReaderT)
import GHC.Generics (Generic)
import Servant (Handler)
import Supple.Database (Pool)
import System.Log.FastLogger (LoggerSet)

data AppConfig = AppConfig
  { environment :: !String
  , shopifyApiRoot :: !String
  , shopifyClientId :: !String
  , shopifyClientSecret :: !String
  , shopifyOAuthAccessToken :: !String
  } deriving (Generic, Show)

data AppCtx = AppCtx
  { _getConfig :: !AppConfig
  , _getLogger :: !LoggerSet
  , _getDbPool :: !Pool
  }

type AppM = ReaderT AppCtx Handler
