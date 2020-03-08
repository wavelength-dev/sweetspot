{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SweetSpot.AppM where

import           Control.Lens
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
  { _configEnvironment :: !Environment
  , _configShopifyClientId :: !Text
  , _configShopifyClientSecret :: !Text
  , _configShopifyOAuthRedirectUri :: !Text
  } deriving (Generic, Show)

makeLenses ''AppConfig

data AppCtx = AppCtx
  { _ctxConfig :: !AppConfig
  , _ctxLogger :: !LoggerSet
  , _ctxDbPool :: !Pool
  }

makeLenses ''AppCtx

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
