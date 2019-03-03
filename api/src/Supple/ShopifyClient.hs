{-# LANGUAGE OverloadedStrings #-}

module Supple.ShopifyClient where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key, values)
import Data.Aeson.Types (Parser, parse)
import Debug.Trace (trace)
import Network.Wreq
import Prelude hiding (product)
import Supple.Data.Api (Image(..), Product(..), Variant(..))
import Supple.Data.Common (Pid(..))

apiRoot =
  "https://e5fe5ceef1de7aef78b0893aaf7ada3b:beefeeb2fc8474121f3de3eac32e026c@libertyprice.myshopify.com/admin"

parseImage :: Value -> Parser Image
parseImage =
  withObject "image" $ \o -> do
    src <- o .: "src"
    return $ Image {_iSrc = src}

parseVariant :: Value -> Parser Variant
parseVariant =
  withObject "variant" $ \o -> do
    id <- o .: "id"
    pid <- o .: "product_id"
    title <- o .: "title"
    sku <- o .: "sku"
    return Variant {_vId = id, _vProductId = pid, _vTitle = title, _vSku = sku}

parseProduct :: Value -> Result Product
parseProduct v = parse parser v
  where
    parser =
      withObject "product" $ \o -> do
        id <- o .: "id"
        title <- o .: "title"
        image <- o .: "image" >>= parseImage
        arr <- o .: "variants"
        variants <- mapM parseVariant arr
        return
          Product
            {_pId = id, _pTitle = title, _pImage = image, _pVariants = variants}

fetchProducts :: IO (Maybe [Product])
fetchProducts = do
  r <- get $ apiRoot ++ "/products.json?fields=id,title,variants,image"
  json <- asValue r
  let result =
        (json ^?! responseBody . key "products") ^.. values & fmap parseProduct &
        sequence
  return $
    case result of
      Success ps -> Just ps
      Error e -> trace e Nothing

fetchProduct :: Pid -> IO Value
fetchProduct (Pid pid) = do
  r <- get $ apiRoot ++ "/products/" ++ show pid ++ ".json"
  json <- asJSON r
  return $ json ^. responseBody

createProduct :: Value -> IO (Maybe Product)
createProduct v = do
  r <- post url v
  json <- asValue r
  let result = json ^?! responseBody . key "product" & parseProduct
  return $
    case result of
      Success p -> Just p
      _ -> Nothing
  where
    url = apiRoot ++ "/products.json"
