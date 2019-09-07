{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SweetSpot.ShopifyClient where

import Prelude hiding (id, product)

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.Aeson.Types (parse)
import qualified Data.ByteString.UTF8 as BLU
import Data.Either (fromRight)
import Data.Text (Text, pack, unpack)
import Data.Text.Read (rational)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Network.Wreq
import SweetSpot.AppM (AppConfig(..), AppCtx(..), AppM)
import SweetSpot.Data.Api (Image(..), Product(..), Variant(..))
import SweetSpot.Data.Common

data StoreVariant = StoreVariant
  { id :: !Int
  , product_id :: !Int
  , title :: !Text
  , sku :: !Text
  , price :: !Text
  } deriving (Generic)

instance FromJSON StoreVariant

data StoreImage = StoreImage
 { src :: !Text
 } deriving (Generic)

instance FromJSON StoreImage

data StoreProduct = StoreProduct
 { id :: !Int
 , title :: !Text
 , image :: !StoreImage
 , variants :: ![StoreVariant]
 } deriving (Generic)

instance FromJSON StoreProduct

toVariant :: StoreVariant -> Variant
toVariant v =
  Variant
    { _vId = Svid . pack . show $ id (v :: StoreVariant)
    , _vProductId = Pid . pack . show . product_id $ v
    , _vTitle = title (v :: StoreVariant)
    , _vSku = Sku . sku $ v
    , _vPrice = Price $ fst $ fromRight (0, "") (rational $ price v)
    }

toProduct :: StoreProduct -> Product
toProduct p =
  Product
    { _pId = Pid . pack . show $ id (p :: StoreProduct)
    , _pTitle = title (p :: StoreProduct)
    , _pVariants = toVariant <$> variants p
    , _pImage = toImage . image $ p
    }
  where
    toImage i = Image { _iSrc = src i }

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
          & traverse (parse parseJSON)
  return $
    case result of
      Success ps -> Just (fmap toProduct ps)
      Error e -> trace e Nothing

fetchProduct :: Pid -> AppM Value
fetchProduct (Pid pid) = do
  apiRoot <- asks (shopifyApiRoot . _getConfig)
  opts <- getOpts
  r <- liftIO $ getWith opts $ apiRoot <> "/products/" <> (unpack pid) <> ".json"
  json <- asJSON r
  return $ json ^. responseBody

createProduct :: Value -> AppM (Result Product)
createProduct v = do
  apiRoot <- asks (shopifyApiRoot . _getConfig)
  opts <- getOpts
  r <- liftIO $ postWith opts (apiRoot <> "/products.json") v
  json <- asValue r
  return $ json ^?! responseBody . key "product" & parse parseJSON & fmap toProduct

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
    accessTokenRoute = shopifyAccessTokenEndpoint config
    body = toJSON $ ExchangeBody
      { client_id = pack $ shopifyClientId config
      , client_secret = pack $ shopifyClientSecret config
      , code = code
      }
    opts = defaults
      & header "Accept" .~ ["application/json"]
      & header "Content-Type" .~ ["application/json"]

  r <- liftIO $ postWith opts accessTokenRoute body
  json <- asValue r
  return $ json ^?! responseBody . key "access_token" . _String
