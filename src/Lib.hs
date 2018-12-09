{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple
  ( Connection
  , Query
  , connect
  , connectDatabase
  , defaultConnectInfo
  , execute
  , query
  , query_
  )
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import GHC.Generics (Generic)
import LoadEnv (loadEnv)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setLogger
  , setPort
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment (lookupEnv)

data BucketReq = BucketReq
  { variant_id :: Int
  , sku :: Text
  , price :: Int
  } deriving (Generic, Show)

data BucketRes = BucketRes
  { message :: Text
  } deriving (Generic, Show)

instance ToJSON BucketReq

instance FromJSON BucketReq

instance ToJSON BucketRes

instance FromRow BucketReq where
  fromRow = BucketReq <$> field <*> field <*> field

type RootAPI
   = "static" :> Raw :<|> "bucket" :> ReqBody '[ JSON] BucketReq :> Post '[ JSON] BucketRes

createBucketHandler :: Connection -> BucketReq -> Handler BucketRes
createBucketHandler dbconn req = do
  liftIO $ execute dbconn q (vid, sk, p)
  return BucketRes {message = "Created experiment"}
  where
    vid = variant_id req
    sk = sku req
    p = price req
    q =
      "insert into buckets (variant_id, sku, price) values (?, ?, ?);" :: Query

server :: Connection -> Server RootAPI
server dbconn = serveDirectoryWebApp "./static" :<|> createBucketHandler dbconn

rootAPI :: Proxy RootAPI
rootAPI = Proxy

createApp :: Connection -> Application
createApp dbconn = serve rootAPI (server dbconn)

runApp :: IO ()
runApp = do
  loadEnv
  apiKey <- lookupEnv "SHOPIFY_API_KEY"
  dbconn <- connect defaultConnectInfo {connectDatabase = "supple"}
  putStrLn $ show apiKey
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8082 $ setLogger aplogger defaultSettings
    runSettings settings (createApp dbconn)
