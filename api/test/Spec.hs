{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  TypeOperators #-}

import qualified Control.Concurrent as C
import Control.Lens hiding (Context)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.Maybe (fromJust)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec
import Test.Hspec.Wai hiding (pending)

import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Route.Injectable (InjectableAPI)
import SweetSpot.Route.Dashboard (DashboardAPI)
import SweetSpot.Server (runServer)

import Database (reset)

magicWaitNumber = 3 * 1000 * 1000

beforeSetup :: IO ()
beforeSetup = runInThread >> C.threadDelay magicWaitNumber
  where
    runInThread = liftIO $ C.forkIO runServer

businessLogicSpec :: Spec
businessLogicSpec =
  beforeAll_ beforeSetup . before_ reset $ do
    let getBucket :<|> postEvent :<|> postLog
          = client (Proxy :: Proxy InjectableAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8082/api"
    manager <- runIO $ newManager defaultManagerSettings
    let
      clientEnv = mkClientEnv manager baseUrl
    describe "GET /api/bucket" $ do
      it "should get buckets for an existing user" $ do
        result <- runClientM (getBucket Nothing (Just 1000)) clientEnv
        case result of
          Left err -> error (show err)
          Right bs -> bs ^?! ix 0 ^. ubUserId `shouldBe` UserId 1000

      it "should create a new user when given a valid campaign id" $ do
        result <- runClientM (getBucket (Just "longv123") Nothing) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()

      it "should assign a new user to either test or control for all buckets" $ do
        result <- runClientM (getBucket (Just "longv123") Nothing) clientEnv
        case result of
          Left err -> error (show err)
          Right buckets -> length uniqTypes `shouldBe` 1
            where
              bucketTypes = buckets ^.. traverse . ubBucketType
              uniqTypes = nub bucketTypes

      it "should not return buckets for invalid campaign ids" $ do
        result <- runClientM (getBucket (Just "unknown_campaign") Nothing) clientEnv
        case result of
          Left (FailureResponse res) -> responseStatusCode res `shouldBe` status400
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should return buckets for invalid campaign ids for known users" $ do
        result <- runClientM (getBucket (Just "unknown_campaign") (Just 1000)) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()

      it "should not return buckets for unknown user ids" $ do
        result <- runClientM (getBucket Nothing (Just 9001)) clientEnv
        case result of
          Left (FailureResponse res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should not return buckets for expired campaign" $ do
        result <- runClientM (getBucket Nothing (Just 1001)) clientEnv
        case result of
          Left (FailureResponse res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should not return buckets for not yet active campaign" $ do
        result <- runClientM (getBucket Nothing (Just 1002)) clientEnv
        case result of
          Left (FailureResponse res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should keep the same user id when given a valid campaignId and userId" $ do
        result <- runClientM (getBucket (Just "longv123") (Just 1000)) clientEnv
        case result of
          Left err -> error (show err)
          Right bs -> bs ^?! ix 0 ^. ubUserId `shouldBe` UserId 1000

      it "should assign existing user to new campaign when old campaigns have expired" $ do
        result <- runClientM (getBucket (Just "longv123") (Just 1001)) clientEnv
        case result of
          Left err -> error (show err)
          Right bs -> bs ^?! ix 1 ^. ubSku `shouldBe` Sku "714449933423"

      it "should not assign existing user to new campaign when old one is still running" $ do
        result <- runClientM (getBucket (Just "somenewcampaign") (Just 1000)) clientEnv
        case result of
          Left err -> error (show err)
          Right bs -> bs ^?! ix 0 ^. ubSku `shouldBe` Sku "714449933422"

    describe "POST /api/event" $ do
      evStr <- runIO $ BS.readFile "./test/data/events.json"
      let evs = fromJust $ JSON.decode evStr :: JSON.Array
          _ :<|> _ :<|> _ :<|> getStats = client (Proxy :: Proxy DashboardAPI)

      it "should get correct impression count" $ do
        traverse_ (\val -> runClientM (postEvent val) clientEnv) evs
        res <- runClientM (getStats "longv123") clientEnv
        case res of
          Right stats -> impressions `shouldBe` 5
            where
              impressions = stats ^. csExperiments ^?! ix 0 ^. esImpressionCount
          Left err -> error (show err)

      it "should track user revenue" $ do
        traverse_ (\val -> runClientM (postEvent val) clientEnv) evs
        res <- runClientM (getStats "longv123") clientEnv
        case res of
          Right stats -> x `shouldBe` 1999.2 -- 8 * 249.90
            where
              x = stats ^. csExperiments ^?! ix 0 ^. esBuckets
                ^?! ix 1 ^. bsUserRevenues ^?! ix 0 ^. _2
          Left err -> error (show err)

main :: IO ()
main = hspec businessLogicSpec
