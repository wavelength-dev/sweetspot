{-# LANGUAGE OverloadedStrings #-}

module Spec.Http (httpSpec) where

import Control.Lens
import Test.Hspec
import Network.Wreq
import Network.Wreq.Types (ResponseChecker)

import Database (reset)
import Util

-- Don't throw exceptions on non 2XX codes
checker :: ResponseChecker
checker req res = return ()

opts = checkResponse ?~ checker $ defaults

apiRoot = "http://localhost:8082"

setup = do
  withEnv [("ENVIRONMENT", "test_http")]
  withApi

httpSpec :: Spec
httpSpec =
  beforeAll_ setup . before_ reset $ do
    describe "GET /health" $
      it "should always return 200" $ do
        res <- getWith opts $ apiRoot <> "/health"
        res ^. responseStatus . statusCode `shouldBe` 200

    describe "GET /api/bucket" $ do
      it "should return 400 with no hmac" $ do
        res <- getWith opts $ apiRoot <> "/api/fulcrum/bucket"
        res ^. responseStatus . statusCode  `shouldBe` 400

      it "should return 400 with incorrect hmac" $ do
        res <- getWith opts $ apiRoot <> "/api/fulcrum/bucket?lol=123&hmac=bal"
        res ^. responseStatus . statusCode `shouldBe` 400

      it "should return 400 with invalid shop domain" $ do
        let hmac = "09f99b5b9ed6ab4b2d75e64fecd8eab1b6fd1b2c326fa1fc9d67f533b19de7a1"
        res <- getWith opts
          $ apiRoot <> "/api/fulcrum/bucket?shop=invalid.myshopify.com&hmac=" <> hmac
        res ^. responseStatus . statusCode `shouldBe` 400

      it "should return 200 with valid hmac and shop domain" $ do
        let
          shopId = "test-shop.myshopify.com"
          cmpId = "6072b6ea-7c37-4b26-80cd-f8f87d05a991"
          hmac = "c3025d4b6029cf8df8478a6b56e0e33a904853f42891d22b857eb34c30815fd3"
        res <- getWith opts $ apiRoot <> "/api/fulcrum/bucket"
          <> "?shop=" <> shopId
          <> "&sscid=" <> cmpId
          <> "&hmac=" <> hmac
        res ^. responseStatus . statusCode `shouldBe` 200
