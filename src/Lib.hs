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
import Network.Wai.Middleware.Cors (simpleCors)
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

data UserBucketReq = UserBucketReq
  { user_id :: Int
  , product_sku :: Text
  } deriving (Generic, Show)

data UserBucketRes = UserBucketRes
  { bucket_price :: Int
  , bucket_variant :: Int
  } deriving (Generic, Show)

instance ToJSON BucketReq

instance FromJSON BucketReq

instance ToJSON BucketRes

instance FromJSON UserBucketReq

instance ToJSON UserBucketRes

instance FromRow BucketReq where
  fromRow = BucketReq <$> field <*> field <*> field

instance FromRow UserBucketRes where
  fromRow = UserBucketRes <$> field <*> field

type RootAPI
   = "static" :> Raw :<|> "bucket" :> ReqBody '[ JSON] BucketReq :> Post '[ JSON] BucketRes :<|> "bucket" :> QueryParam "uid" Int :> QueryParam "sku" Text :> Get '[JSON] UserBucketRes

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

getUserBucketHandler :: Connection -> Maybe Int -> Maybe Text -> Handler UserBucketRes
getUserBucketHandler dbconn (Just uid) (Just sku) = do
  res <- liftIO $ (query dbconn q (uid, sku) :: IO [UserBucketRes])
  return $ res !! 0
  where
    q = "select buckets.price, buckets.variant_id from user_buckets inner join users on user_buckets.user_id = users.user_id inner join buckets on user_buckets.variant_id = buckets.variant_id where users.user_id = ? and buckets.sku = ?;" :: Query
getUserBucketHandler _ _ _ = undefined

server :: Connection -> Server RootAPI
server dbconn = serveDirectoryWebApp "./static" :<|> createBucketHandler dbconn :<|> getUserBucketHandler dbconn

rootAPI :: Proxy RootAPI
rootAPI = Proxy

createApp :: Connection -> Application
createApp dbconn = simpleCors $ serve rootAPI (server dbconn)

runApp :: IO ()
runApp = do
  loadEnv
  apiKey <- lookupEnv "SHOPIFY_API_KEY"
  dbconn <- connect defaultConnectInfo {connectDatabase = "supple"}
  putStrLn $ show apiKey
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8082 $ setLogger aplogger defaultSettings
    runSettings settings (createApp dbconn)
