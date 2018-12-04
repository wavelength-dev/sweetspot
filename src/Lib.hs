{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( runApp
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import GHC.Generics
import LoadEnv
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.StaticFiles
import System.Environment (lookupEnv)
import System.IO

type RootAPI
   = "hello" :> Get '[ PlainText] String :<|> "static" :> Raw :<|> "experiments" :> Get '[ JSON] [Experiment]

data Experiment = Experiment
  { id :: Int
  , price :: Int
  } deriving (Generic, Show)

instance ToJSON Experiment

instance FromRow Experiment where
  fromRow = Experiment <$> field <*> field

helloWorldMessage :: String
helloWorldMessage = "Hello World!"

experimentsHandler :: Connection -> Handler [Experiment]
experimentsHandler dbconn =
  liftIO (query_ dbconn experimentsQuery :: IO [Experiment])
  where
    experimentsQuery = "select product_id, price from experiments;" :: Query

server :: Connection -> Server RootAPI
server dbconn =
  return helloWorldMessage :<|> serveDirectoryWebApp "./static" :<|>
  experimentsHandler dbconn

rootAPI :: Proxy RootAPI
rootAPI = Proxy

createApp :: Connection -> Application
createApp dbconn = serve rootAPI (server dbconn)

runApp :: IO ()
runApp = do
  loadEnv
  apiKey <- lookupEnv "SHOPIFY_API_KEY"
  dbconn <-
    connect
      defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "supple"
        , connectPort = 5432
        , connectUser = "postgres"
        }
  putStrLn $ show apiKey
  run 8082 (createApp dbconn)
