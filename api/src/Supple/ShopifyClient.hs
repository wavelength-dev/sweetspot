{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Supple.ShopifyClient where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Aeson.Types (Parser, parse)
import Data.Text (Text)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.Wreq
import Prelude hiding (product)
import Supple.Data.Api (Image(..), Product(..), Variant(..))
import Supple.Data.Common (Pid(..))

apiRoot =
  "https://libertyprice.myshopify.com/admin"

opts = defaults & header "X-Shopify-Access-Token" .~ ["705fdddbc12654ca0ecf353e0b2421d5"]

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
  r <- getWith opts $ apiRoot ++ "/products.json?fields=id,title,variants,image"
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
  r <- getWith opts $ apiRoot ++ "/products/" ++ show pid ++ ".json"
  json <- asJSON r
  return $ json ^. responseBody

createProduct :: Value -> IO (Maybe Product)
createProduct v = do
  r <- postWith opts url v
  json <- asValue r
  let result = json ^?! responseBody . key "product" & parseProduct
  return $
    case result of
      Success p -> Just p
      _ -> Nothing
  where
    url = apiRoot ++ "/products.json"


data ExchangeBody = ExchangeBody
  { client_id :: Text
  , client_secret :: Text
  , code :: Text
  } deriving (Generic)

instance ToJSON ExchangeBody

exchangeAccessToken :: Text -> IO Text
exchangeAccessToken code = do
  r <- post (apiRoot ++ "/oauth/access_token") body
  json <- asValue r
  return $ json ^?! responseBody . key "access_token" . _String
  where
    body = toJSON $ ExchangeBody
      { client_id = "634b531a6568d6eb076c2ad5c7e0265a"
      , client_secret = "bd382d7ebb6c489bb24de3aefdb2498d"
      , code = code
      }
