{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

import qualified Control.Concurrent as C
import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Lens hiding (Context)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
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

-- we can spin up a server in another thread and kill that thread when done
-- in an exception-safe way
withUserApp :: IO () -> IO ()
withUserApp action =
  bracket (liftIO $ C.forkIO $ runServer) C.killThread (const action)

businessLogicSpec :: Spec
businessLogicSpec =
  -- `around` will start our Server before the tests and turn it off after
  around_ withUserApp $ do
    let getBucket :<|> postEvent :<|> postLog
          = client (Proxy :: Proxy InjectableAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8082/api"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    -- testing scenarios start here
    describe "GET /api/bucket" $ do
      it "should get bucket for existing user" $ do
        result <- runClientM (getBucket (Just 1)) clientEnv
        result ^?! _Right ^.  ubUserId `shouldBe` UserId 1

      it "should create a new user when no query param" $ do
        result <- runClientM (getBucket Nothing) clientEnv
        result ^?! _Right ^.  ubSku `shouldBe` Sku "3"

      it "should create a new user when no query param" $ do
        result <- runClientM (getBucket Nothing) clientEnv
        result ^?! _Right ^.  ubSku `shouldBe` Sku "3"

main :: IO ()
main = hspec businessLogicSpec
