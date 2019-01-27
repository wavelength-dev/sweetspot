{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App
  ( runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , defaultOptions
  , encode
  , genericToEncoding
  , toEncoding
  )
import Data.Default (def)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Servant
import System.Log.FastLogger
  ( LoggerSet
  , ToLogStr(..)
  , defaultBufSize
  , newStdoutLoggerSet
  , pushLogStrLn
  )

import Database.Lib
  ( Connection
  , getDbConnection
  , getExperimentBuckets
  , getNewUserBuckets
  , getUserBuckets
  )
import ShopifyClient (createVariant, fetchProducts)
import Types

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
  { logMessage :: !Text
  , timestamp :: !UTCTime
  } deriving (Eq, Show, Generic)

instance FromJSON LogMessage

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

type AppM = ReaderT AppCtx Handler

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> Get '[ JSON] [UserBucket]

type CreateVariantRoute
   = "variant" :> QueryParam "pid" Int :> ReqBody '[ JSON] CreateVariant :> Post '[ JSON] OkResponse

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type RootAPI
   = UserBucketRoute :<|> CreateVariantRoute :<|> ProductsRoute :<|> ExperimentsRoute

rootAPI :: Proxy RootAPI
rootAPI = Proxy

getUserBucketHandler :: Maybe Int -> AppM [UserBucket]
getUserBucketHandler (Just uid) = do
  dbconn <- asks _getDbConn
  logset <- asks _getLogger
  ts <- liftIO getCurrentTime
  res <- liftIO $ getUserBuckets dbconn uid
  liftIO $
    pushLogStrLn logset $
    toLogStr LogMessage {logMessage = "Got user bucket", timestamp = ts}
  return res
getUserBucketHandler Nothing = do
  dbconn <- asks _getDbConn
  res <- liftIO $ getNewUserBuckets dbconn
  return res

createVariantHandler :: Maybe Int -> CreateVariant -> AppM OkResponse
createVariantHandler (Just pid) var = do
  _ <- liftIO $ createVariant pid var
  return $ OkResponse {message = "Created variant"}
createVariantHandler _ _ = throwError err500 {errBody = "Something went wrong"}

getProductsHandler :: AppM [Product]
getProductsHandler = liftIO fetchProducts

getExperimentsHandler :: AppM [ExperimentBuckets]
getExperimentsHandler = do
  dbconn <- asks _getDbConn
  liftIO $ getExperimentBuckets dbconn

server :: ServerT RootAPI AppM
server =
  getUserBucketHandler :<|> createVariantHandler :<|> getProductsHandler :<|>
  getExperimentsHandler

createApp :: AppCtx -> Application
createApp ctx =
  simpleCors $ serve rootAPI $ hoistServer rootAPI (flip runReaderT ctx) server

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $
  def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}

runApp :: IO ()
runApp = do
  dbconn <- getDbConnection
  warpLogger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize
  let config = AppConfig "dev" "0.1"
      ctx = AppCtx config appLogger dbconn
  Warp.run 8082 $ warpLogger $ createApp ctx
