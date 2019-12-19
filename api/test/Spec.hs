{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  TypeOperators #-}

import qualified Control.Concurrent as C
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.List (nub, find)
import Data.Maybe (fromJust, isJust)
import Data.UUID (fromText)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec
import Test.Hspec.Wai hiding (pending)

import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Route.Injectable (InjectableAPI)
-- import SweetSpot.Route.Dashboard (DashboardAPI)
import SweetSpot.Server (runServer)

import Database (reset)

magicWaitNumber = 3 * 1000 * 1000

toUUID = fromJust . fromText

shopDomain = ShopDomain "test-shop.myshopify.com"
invalidDomain = ShopDomain "lol-shop.myshopify.com"

user1 = toUUID "2eb6a046-6609-4518-ab23-87f1ad56bbaa"
user2 = toUUID "e3b937e7-ac65-4324-9d67-040cdc35b555"
user3 = toUUID "85271f15-683b-4972-bd68-b7aaacdeb70d"
unknownUser = toUUID "8a2492c7-82f8-4845-844a-00589d270f66"

campaign1 = toUUID "6072b6ea-7c37-4b26-80cd-f8f87d05a991"
campaign2 = toUUID "6072b6ea-7c37-4b26-80cd-f8f87d05a992"
campaign3 = toUUID "6072b6ea-7c37-4b26-80cd-f8f87d05a993"
unknownCampaign = toUUID "fec505ce-4100-4c3f-a55b-608b14688c52"

beforeSetup :: IO ()
beforeSetup = runInThread >> C.threadDelay magicWaitNumber
  where
    runInThread = liftIO $ C.forkIO runServer

businessLogicSpec :: Spec
businessLogicSpec =
  beforeAll_ beforeSetup . before_ reset $ do
    let getTest :<|> postCheckout = client (Proxy :: Proxy InjectableAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8082/api"
    manager <- runIO $ newManager defaultManagerSettings
    let
      clientEnv = mkClientEnv manager baseUrl
    describe "GET /api/bucket" $ do
      it "should not get tests for invalid shop domain" $ do
        result <- runClientM (getTest (Just invalidDomain) (Just campaign1) (Just user1)) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status400
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should get buckets for an existing user" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign1) (Just user1)) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> (userId . head $ tms)  `shouldBe` UserId user1

      it "should create a new user when given a valid campaign id" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign1) Nothing) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()

      it "should not return buckets for invalid campaign ids" $ do
        result <- runClientM (getTest (Just shopDomain) (Just unknownCampaign) Nothing) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status400
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should return buckets for invalid campaign ids for known users" $ do
        result <- runClientM (getTest (Just shopDomain) (Just unknownCampaign) (Just user1)) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()

      it "should not return buckets for unknown user ids" $ do
        result <- runClientM (getTest (Just shopDomain) Nothing (Just unknownUser)) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should not return buckets for expired campaign" $ do
        result <- runClientM (getTest (Just shopDomain) Nothing (Just campaign2)) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should not return buckets for not yet active campaign" $ do
        result <- runClientM (getTest (Just shopDomain) Nothing (Just campaign3)) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should keep the same user id when given a valid campaignId and userId" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign1) (Just user1)) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> uniqUserIds `shouldBe` 1
            where
              uniqUserIds = length . nub $ filter (== UserId user1) $ fmap userId tms

      it "should assign existing user to new campaign when old campaigns have expired" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign1) (Just user2)) clientEnv
        case result of
          Left err -> error (show err)
          Right tms ->
            isJust (find ((== Sku "black_wb_sku") . sku) tms) `shouldBe` True

      it "should not assign existing user to new campaign when old one is still running" $ do
        result <- runClientM (getTest (Just shopDomain) (Just unknownCampaign) (Just user1)) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> (sku . head $ tms) `shouldBe` Sku "black_wb_sku"

    describe "POST /api/checkout" $ do
      validEvStr <- runIO $ BS.readFile "./test/data/valid_event.json"
      let validEv = fromJust $ JSON.decode validEvStr :: ApiCheckoutEvent

      it "should accept valid event" $ do
        result <- runClientM (postCheckout (Just shopDomain) validEv) clientEnv
        case result of
          Left err -> error $ show err
          Right _ -> return ()


main :: IO ()
main = hspec businessLogicSpec
