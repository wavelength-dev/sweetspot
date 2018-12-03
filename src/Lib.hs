{-# LANGUAGE DataKinds #-}

module Lib
  ( runApp
  ) where

import GHC.Generics
import LoadEnv
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)
import System.IO

type RootAPI = Get '[ PlainText] String

helloWorldMessage :: String
helloWorldMessage = "Hello World!"

server1 :: Server RootAPI
server1 = return helloWorldMessage

rootAPI :: Proxy RootAPI
rootAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve rootAPI server1

runApp :: IO ()
runApp = do
  loadEnv
  apiKey <- lookupEnv "SHOPIFY_API_KEY"
  putStrLn $ show apiKey
  run 8082 app
