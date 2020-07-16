{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.AppM where

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import GHC.Generics (Generic)
import RIO hiding (Handler)
import Servant
  ( Handler,
    ServerError,
  )
import SweetSpot.Database (Pool)
import SweetSpot.Env (Environment)
import System.Log.FastLogger (LoggerSet)

data AppConfig
  = AppConfig
      { _configEnvironment :: !Environment,
        _configShopifyClientId :: !Text,
        _configShopifyClientSecret :: !Text,
        _configShopifyOAuthRedirectUri :: !Text,
        _configSweetSpotApiRoot :: !Text
      }
  deriving (Generic, Show)

makeLenses ''AppConfig

data AppCtx
  = AppCtx
      { _ctxConfig :: !AppConfig,
        _ctxLogger :: !LoggerSet,
        _ctxDbPool :: !Pool
      }

makeLenses ''AppCtx

type ServerM = ReaderT AppCtx Handler

newtype AppM a
  = AppM
      { runAppM :: ServerM a
      }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError ServerError,
      MonadReader AppCtx,
      MonadThrow
    )
