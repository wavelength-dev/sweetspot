{-# LANGUAGE OverloadedStrings #-}

module ShopifyClient where

import Control.Lens
import Data.Aeson (toJSON)
import Network.Wreq

import Types

apiRoot =
  "https://e5fe5ceef1de7aef78b0893aaf7ada3b:beefeeb2fc8474121f3de3eac32e026c@libertyprice.myshopify.com/admin"

fetchProducts :: IO ShopifyResponse
fetchProducts = do
  r <- get $ apiRoot ++ "/products.json?fields=id,title,variants"
  json <- asJSON r
  return $ json ^. responseBody

createVariant :: Int -> CreateVariant -> IO ()
createVariant pid var = do
  r <- post url body
  return ()
  where
    url = apiRoot ++ "/products/" ++ show pid ++ "/variants.json"
    body = toJSON ShopifyVariantBody {variant = var}
