{-# LANGUAGE DeriveGeneric #-}

module SweetSpot.AppM where

import Control.Monad.Reader (ReaderT)
import GHC.Generics (Generic)
import Servant (Handler)
import SweetSpot.Database (Pool)
import System.Log.FastLogger (LoggerSet)

import qualified Database.Beam.Postgres as PG
import qualified Data.Pool as P

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
  , _getNewDbPool :: !(P.Pool PG.Connection)
  }

type AppM = ReaderT AppCtx Handler
