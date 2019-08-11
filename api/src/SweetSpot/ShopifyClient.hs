{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SweetSpot.ShopifyClient where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Aeson.Types (Parser, parse)
import qualified Data.ByteString.UTF8 as BLU
import Data.Either (fromRight)
import Data.Text (Text, pack)
import Data.Text.Read (rational)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.Wreq
import Prelude hiding (product)
import SweetSpot.AppM (AppConfig(..), AppCtx(..), AppM)
import SweetSpot.Data.Api (Image(..), Product(..), Variant(..))
import SweetSpot.Data.Common (Pid(..), Price(..))

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
    price <- o .: "price"
    return Variant
      { _vId = id
      , _vProductId = pid
      , _vTitle = title
      , _vSku = sku
      , _vPrice = Price $ fst $ fromRight (0, "") (rational price)}

parseProduct :: Value -> Result Product
parseProduct = parse parser
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

getOpts :: AppM Network.Wreq.Options
getOpts = do
  oauthToken <- asks (shopifyOAuthAccessToken . _getConfig)
  let token = BLU.fromString oauthToken
  return $ defaults & header "X-Shopify-Access-Token" .~ [token]


fetchProducts :: AppM (Maybe [Product])
fetchProducts = do
  apiRoot <- asks (shopifyApiRoot . _getConfig)
  opts <- getOpts
  r <- liftIO $ getWith opts $ apiRoot <> "/products.json?fields=id,title,variants,image"
  json <- asValue r
  let result =
        (json ^?! responseBody . key "products") ^.. values
          & traverse parseProduct
  return $
    case result of
      Success ps -> Just ps
      Error e -> trace e Nothing

fetchProduct :: Pid -> AppM Value
fetchProduct (Pid pid) = do
  apiRoot <- asks (shopifyApiRoot . _getConfig)
  opts <- getOpts
  r <- liftIO $ getWith opts $ apiRoot <> "/products/" <> show pid <> ".json"
  json <- asJSON r
  return $ json ^. responseBody

createProduct :: Value -> AppM (Result Product)
createProduct v = do
  apiRoot <- asks (shopifyApiRoot . _getConfig)
  opts <- getOpts
  r <- liftIO $ postWith opts (apiRoot <> "/products.json") v
  json <- asValue r
  return $ json ^?! responseBody . key "product" & parseProduct

data ExchangeBody = ExchangeBody
  { client_id :: Text
  , client_secret :: Text
  , code :: Text
  } deriving (Generic)

instance ToJSON ExchangeBody

exchangeAccessToken :: Text -> AppM Text
exchangeAccessToken code = do
  config <- asks _getConfig
  let
    apiRoot = shopifyApiRoot config
    body = toJSON $ ExchangeBody
      { client_id = pack $ shopifyClientId config
      , client_secret = pack $ shopifyClientSecret config
      , code = code
      }
  r <- liftIO $ post (apiRoot <> "/oauth/access_token") body
  json <- asValue r
  return $ json ^?! responseBody . key "access_token" . _String
