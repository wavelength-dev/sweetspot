{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import LoadEnv (loadEnv)
import Network.Wai (Middleware, Request)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setLogger
  , setPort
  )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy
  , cors
  , corsExposedHeaders
  , corsMethods
  , corsOrigins
  , corsRequestHeaders
  , simpleCors
  , simpleCorsResourcePolicy
  , simpleMethods
  , simpleResponseHeaders
  )
import Servant
import System.Environment (lookupEnv)

import Db (Connection, getDbConnection, getUserBucket, insertBucket)
import Types

type CookieHeader = '[ Header "Set-Cookie" Text]

type StaticRoute = "static" :> Raw

type CreateBucketRoute
   = "bucket" :> ReqBody '[ JSON] Bucket :> Post '[ JSON] BucketRes

type UserBucketRoute
   = "bucket" :> QueryParam "uid" Int :> QueryParam "sku" Text :> Get '[ JSON] (Headers CookieHeader UserBucket)

type RootAPI = StaticRoute :<|> CreateBucketRoute :<|> UserBucketRoute

createBucketHandler :: Connection -> Bucket -> Handler BucketRes
createBucketHandler dbconn req = do
  liftIO $ insertBucket dbconn req
  return BucketRes {message = "Created experiment"}

getUserBucketHandler ::
     Connection
  -> Maybe Int
  -> Maybe Text
  -> Handler (Headers CookieHeader UserBucket)
getUserBucketHandler dbconn (Just uid) (Just sku) = do
  res <- liftIO $ getUserBucket dbconn uid sku
  case res of
    (Just res) -> return $ addHeader "lol=bal" $ res
    Nothing -> throwError err500 {errBody = "Something went wrong"}
getUserBucketHandler _ _ _ =
  throwError err500 {errBody = "Something went wrong"}

server :: Connection -> Server RootAPI
server dbconn =
  serveDirectoryWebApp "./static" :<|> createBucketHandler dbconn :<|>
  getUserBucketHandler dbconn

rootAPI :: Proxy RootAPI
rootAPI = Proxy

corsMiddleware :: Request -> Maybe CorsResourcePolicy
corsMiddleware _ =
  Just $
  simpleCorsResourcePolicy
    { corsOrigins =
        Just (["https://libertyproduct.myshopify.com"], True)
    , corsExposedHeaders =
        Just ["Set-Cookie", "Access-Control-Allow-Origin", "Content-Type"]
    }

createApp :: Connection -> Application
createApp dbconn = cors corsMiddleware $ serve rootAPI (server dbconn)

runApp :: IO ()
runApp = do
  loadEnv
  apiKey <- lookupEnv "SHOPIFY_API_KEY"
  dbconn <- getDbConnection
  putStrLn $ show apiKey
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8082 $ setLogger aplogger defaultSettings
    runSettings settings (createApp dbconn)
