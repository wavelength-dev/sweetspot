{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SweetSpot.AppM where

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.Reader.Class     ( MonadReader )
import           Control.Monad.IO.Class         ( MonadIO(..) )

import           GHC.Generics                   ( Generic )
import           Servant                        ( Handler
                                                , ServerError
                                                )
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

type ServerM = ReaderT AppCtx Handler

newtype AppM a = AppM
  { runAppM :: ServerM a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ServerError
    , MonadReader AppCtx
    , MonadThrow
    )
