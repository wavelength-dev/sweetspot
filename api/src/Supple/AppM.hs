{-# LANGUAGE DeriveGeneric #-}

module Supple.AppM where

import Control.Monad.Reader (ReaderT)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , defaultOptions
  , encode
  , genericToEncoding
  , toEncoding
  )
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant (Handler)
import Supple.Database (Connection)
import System.Log.FastLogger (LoggerSet)
import System.Log.FastLogger (ToLogStr(..))

data AppConfig = AppConfig
  { environment :: !Text
  , version :: !Text
  } deriving (Generic, Show)

data AppCtx = AppCtx
  { _getConfig :: !AppConfig
  , _getLogger :: !LoggerSet
  , _getDbConn :: !Connection
  }

data LogMessage = LogMessage
  { message :: !Text
  , timestamp :: !UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

type AppM = ReaderT AppCtx Handler
