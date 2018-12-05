{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( runApp
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
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

type RootAPI
   = "hello" :> Get '[ PlainText] Text :<|> "static" :> Raw :<|> "experiments" :> Get '[ JSON] [Experiment] :<|> "experiment" :> QueryParam "id" Int :> QueryParam "price" Int :> Post '[ JSON] Response

data Experiment = Experiment
  { id :: Int
  , price :: Int
  } deriving (Generic, Show)

instance ToJSON Experiment

data Response = Response
  { message :: Text
  } deriving (Generic)

instance ToJSON Response

instance FromRow Experiment where
  fromRow = Experiment <$> field <*> field

helloWorldMessage :: Text
helloWorldMessage = "Hello World!"

experimentsHandler :: Connection -> Handler [Experiment]
experimentsHandler dbconn =
  liftIO (query_ dbconn experimentsQuery :: IO [Experiment])
  where
    experimentsQuery = "select product_id, price from experiments;" :: Query

createExperimentHandler ::
     Connection -> Maybe Int -> Maybe Int -> Handler Response
createExperimentHandler dbconn (Just id) (Just price) = do
  liftIO $ execute dbconn q (id, price)
  return Response {message = "Created experiment"}
  where
    q = "insert into experiments (product_id, price) values (?, ?)" :: Query
createExperimentHandler _ _ _ = return Response {message = "Invalid request"}

server :: Connection -> Server RootAPI
server dbconn =
  return helloWorldMessage :<|> serveDirectoryWebApp "./static" :<|>
  experimentsHandler dbconn :<|>
  createExperimentHandler dbconn

rootAPI :: Proxy RootAPI
rootAPI = Proxy

createApp :: Connection -> Application
createApp = serve rootAPI . server

runApp :: IO ()
runApp = do
  loadEnv
  apiKey <- lookupEnv "SHOPIFY_API_KEY"
  dbconn <- connect defaultConnectInfo {connectDatabase = "supple"}
  putStrLn $ show apiKey
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8082 $ setLogger aplogger defaultSettings
    runSettings settings (createApp dbconn)
