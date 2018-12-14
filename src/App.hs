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
import Network.Wai.Middleware.Cors
  (
    cors
  , corsExposedHeaders
  , corsOrigins
  , simpleCorsResourcePolicy
  )
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

import Database (Connection, getDbConnection, getUserBucket, insertBucket)
import Types

data AppConfig = AppConfig
  { environment :: !Text
  , version :: !Text
  } deriving (Generic, Show)

data AppCtx = AppCtx
  { _getConfig :: AppConfig
  , _getLogger :: LoggerSet
  , _getDbConn :: Connection
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

type CookieHeader = '[ Header "Set-Cookie" Text]

type StaticRoute = "static" :> Raw

type CreateBucketRoute
   = "bucket" :> ReqBody '[ JSON] Bucket :> Post '[ JSON] BucketRes

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> QueryParam "sku" Text :> Get '[ JSON] (Headers CookieHeader UserBucket)

type RootAPI = StaticRoute :<|> CreateBucketRoute :<|> UserBucketRoute

createBucketHandler :: Bucket -> AppM BucketRes
createBucketHandler req = do
  dbconn <- asks _getDbConn
  liftIO $ insertBucket dbconn req
  return BucketRes {message = "Created experiment"}

getUserBucketHandler ::
     Maybe Int -> Maybe Text -> AppM (Headers CookieHeader UserBucket)
getUserBucketHandler (Just uid) (Just sku) = do
  dbconn <- asks _getDbConn
  logset <- asks _getLogger
  ts <- liftIO getCurrentTime
  res <- liftIO $ getUserBucket dbconn uid sku
  liftIO $
    pushLogStrLn logset $
    toLogStr LogMessage {logMessage = "Got user bucket", timestamp = ts}
  return $ addHeader "lol=bal" $ res
getUserBucketHandler _ _ = throwError err500 {errBody = "Something went wrong"}

server :: ServerT RootAPI AppM
server =
  serveDirectoryWebApp "./static" :<|> createBucketHandler :<|>
  getUserBucketHandler

rootAPI :: Proxy RootAPI
rootAPI = Proxy

corsMiddleware :: Middleware
corsMiddleware =
  cors $ \_ ->
    Just $
    simpleCorsResourcePolicy
      { corsOrigins = Just (["https://libertyproduct.myshopify.com"], True)
      , corsExposedHeaders =
          Just ["Set-Cookie", "Access-Control-Allow-Origin", "Content-Type"]
      }

createApp :: AppCtx -> Application
createApp ctx =
  corsMiddleware $
  serve rootAPI $ hoistServer rootAPI (flip runReaderT ctx) server

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
