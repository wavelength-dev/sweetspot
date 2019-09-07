{-# LANGUAGE DeriveGeneric #-}

module SweetSpot.AppM where

import           Control.Monad.Reader           ( ReaderT )
import           GHC.Generics                   ( Generic )
import           Servant                        ( Handler )
import           System.Log.FastLogger          ( LoggerSet )

import           SweetSpot.Database             ( Pool )

data AppConfig = AppConfig
  { environment :: !String
  , shopifyApiRoot :: !String
  , shopifyAccessTokenEndpoint :: !String
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
