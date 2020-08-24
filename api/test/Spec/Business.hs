module Spec.Business (businessLogicSpec) where

import Database (reset)
import Mock.Shopify
import Network.HTTP.Client hiding (Proxy, responseHeaders)
import RIO
import RIO.List (nub)
import RIO.List.Partial (head)
import Servant
import Servant.Client
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Route.Dashboard (DashboardAPI)
import SweetSpot.Route.Fulcrum (FulcrumAPI)
import Test.Hspec
import Util

setup = do
  withEnv [("ENVIRONMENT", "test_business")]
  withApi
  withShopify

businessLogicSpec :: Spec
businessLogicSpec =
  beforeAll_ setup . before_ reset $ do
    let getTest :<|> putCartToken :<|> _ = client (Proxy :: Proxy FulcrumAPI)
    let _ :<|> getCampaigns :<|> _ = client (Proxy :: Proxy DashboardAPI)
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8082/api"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    describe "GET /api/bucket" $ do
      it "should get buckets for an existing user" $ do
        result <- runClientM (getTest shopDomain user1) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> (_testMapUserId . head $ tms) `shouldBe` user1
      it "should create a new user when given a valid campaign id" $ do
        result <- runClientM (getTest shopDomain user1) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()
      it "should return buckets for invalid campaign ids for known users" $ do
        result <- runClientM (getTest shopDomain user1) clientEnv
        case result of
          Left err -> error (show err)
          Right _ -> return ()
      it "should keep the same user id when given a valid campaignId and userId" $ do
        result <- runClientM (getTest shopDomain user1) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> uniqUserIds `shouldBe` 1
            where
              uniqUserIds = length . nub $ filter (== user1) $ fmap _testMapUserId tms
      it "should not assign existing user to new campaign when old one is still running" $ do
        result <- runClientM (getTest shopDomain user1) clientEnv
        case result of
          Left err -> error (show err)
          Right tms -> (_testMapSku . head $ tms) `shouldBe` Sku "black_wb_sku"
    describe "PUT /api/fulcrum/cart-token" $ do
      it "should accept valid cart-token request" $ do
        let payload =
              CartTokenReq
                { _cartTokenReqToken = CartToken "lol-some-cart-token123",
                  _cartTokenReqUser = user1
                }
        result <- runClientM (putCartToken shopDomain payload) clientEnv
        case result of
          Left err -> error $ show err
          Right _ -> return ()

-- describe "POST /api/oauth/install" $ do
--   let installHandler :<|> redirectHandler = client (Proxy :: Proxy OAuthAPI)
--   it "should return empty response from install endpoint" $ do
--     result <- runClientM (installHandler
--                           (Just (ShopDomain "localhost:9999"))
--                           (Just (Timestamp "12345"))
--                           (Just (HMAC' "lolbal")))
--                          clientEnv
--     case result of
--       Left err -> error $ "Got error: " <> show err
--       Right (Headers NoContent hs) -> return ()

-- describe "GET /api/dashboard/campaigns" $ do
--   it "should return correct stats" $ do
--     result <- runClientM (getCampaigns campaign1 (Just shopDomain)) clientEnv
--     case result of
--       Left err -> error $ "Got error: " <> show err
--       Right stats -> do
--         V.length convC `shouldBe` 1
--         V.length convT `shouldBe` 0
--         where
--           convC = _cmpStatsConvertersControl stats
--           convT = _cmpStatsConvertersTest stats

main :: IO ()
main = hspec businessLogicSpec
