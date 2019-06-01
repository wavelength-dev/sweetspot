{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Lens hiding (Context)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.List.Lens
import Data.Text (Text, unpack)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.Client
import Servant.Server
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher

import Supple.Data.Api
import Supple.Data.Common
import Supple.Route.Injectable (InjectableAPI)
import Supple.Server (rootAPI, runServer)

import Database (reset)

magicWaitNumber = 3 * 1000 * 1000

beforeSetup :: IO ()
beforeSetup = runInThread >> C.threadDelay magicWaitNumber
  where
    runInThread = liftIO $ C.forkIO $ runServer

businessLogicSpec :: Spec
businessLogicSpec =
  -- `around` will start our Server before the tests and turn it off after
  beforeAll_ beforeSetup $ before_ reset $ do
    let getBucket :<|> postEvent :<|> postLog
          = client (Proxy :: Proxy InjectableAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8082/api"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    -- testing scenarios start here
    describe "GET /api/bucket" $ do
      it "should get bucket for existing user" $ do
        result <- runClientM (getBucket Nothing (Just 1000)) clientEnv
        case result of
          Right bs -> bs ^?! ix 0 ^. ubUserId `shouldBe` (UserId 1000)
          Left err -> do
            print err
            True `shouldBe` False

main :: IO ()
main = hspec businessLogicSpec
