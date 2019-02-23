{-# LANGUAGE OverloadedStrings #-}

module Supple.ShopifyClient where

import Control.Lens
import Data.Aeson (Value)
import Network.Wreq
import Prelude hiding (product)
import Supple.Data.Api (Product)
import Supple.Data.Shopify (ShopifyProductResponse(..), ShopifyResponse(..))

apiRoot =
  "https://e5fe5ceef1de7aef78b0893aaf7ada3b:beefeeb2fc8474121f3de3eac32e026c@libertyprice.myshopify.com/admin"

fetchProducts :: IO [Product]
fetchProducts = do
  r <- get $ apiRoot ++ "/products.json?fields=id,title,variants,image"
  json <- asJSON r
  return $ products (json ^. responseBody)

fetchProduct :: Int -> IO Value
fetchProduct pid = do
  r <- get $ apiRoot ++ "/products/" ++ show pid ++ ".json"
  json <- asJSON r
  return $ json ^. responseBody

createProduct :: Value -> IO Product
createProduct v = do
  r <- post url v
  json <- asJSON r
  return $ product (json ^. responseBody)
  where
    url = apiRoot ++ "/products.json"
