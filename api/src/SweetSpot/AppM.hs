{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SweetSpot.AppM where

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( ReaderT )
import           Control.Monad.Reader.Class     ( MonadReader )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Servant                        ( Handler
                                                , ServerError
                                                )
import           System.Log.FastLogger          ( LoggerSet )
import           SweetSpot.Database             ( Pool )
import           SweetSpot.Env                  ( Environment )

data AppConfig = AppConfig
  { environment :: !Environment
  , shopifyClientId :: !Text
  , shopifyClientSecret :: !Text
  , shopifyOAuthRedirectUri :: !Text
  , basicAuthUser :: !Text
  , basicAuthPassword :: !Text
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
