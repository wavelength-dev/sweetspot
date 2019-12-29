{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  TypeOperators #-}

module Spec.Business (businessLogicSpec) where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BS
import Data.List (nub, find)
import Data.Maybe (fromJust, isJust)
import Network.HTTP.Client hiding (Proxy, responseHeaders)
import Network.HTTP.Types hiding (Header, responseHeaders)
import Servant
import Servant.Client
import Test.Hspec

import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Route.Injectable (InjectableAPI)
import SweetSpot.Route.OAuth (OAuthAPI)

import Database (reset)
import Mock.Shopify
import Util

setup = do
  withEnv [("ENVIRONMENT", "test_business")]
  withApi
  withShopify

businessLogicSpec :: Spec
businessLogicSpec =
  beforeAll_ setup . before_ reset $ do
    let getTest :<|> postCheckout = client (Proxy :: Proxy InjectableAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8082/api"
    manager <- runIO $ newManager defaultManagerSettings
    let
      clientEnv = mkClientEnv manager baseUrl
    describe "GET /api/bucket" $ do
      it "should get buckets for an existing user" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign1) (Just user1)) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> (userId . head $ tms)  `shouldBe` user1

      it "should create a new user when given a valid campaign id" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign1) Nothing) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()

      it "should not return buckets for invalid campaign ids" $ do
        result <- runClientM (getTest (Just shopDomain) (Just unknownCampaign) Nothing) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status404
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
        result <- runClientM (getTest (Just shopDomain) (Just campaign2) Nothing) clientEnv
        case result of
          Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` status404
          Left err -> error (show err)
          Right _ -> expectationFailure "expected request to fail"

      it "should not return buckets for not yet active campaign" $ do
        result <- runClientM (getTest (Just shopDomain) (Just campaign3) Nothing) clientEnv
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
              uniqUserIds = length . nub $ filter (== user1) $ fmap userId tms

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

    describe "POST /api/oauth/install" $ do
      let installHandler :<|> redirectHandler = client (Proxy :: Proxy OAuthAPI)
      it "should return empty response from install endpoint" $ do
        result <- runClientM (installHandler
                              (Just (ShopDomain "localhost:9999"))
                              (Just (Timestamp "12345"))
                              (Just (HMAC' "lolbal")))
                             clientEnv
        case result of
          Left err -> error $ "Got error: " <> show err
          Right (Headers NoContent hs) -> return ()



main :: IO ()
main = hspec businessLogicSpec
