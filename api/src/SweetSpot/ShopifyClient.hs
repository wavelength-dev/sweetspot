{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.ShopifyClient where

import           Prelude                 hiding ( id
                                                , product
                                                )

import           Control.Lens
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Control.Monad.Reader.Class     ( asks
                                                , MonadReader(..)
                                                )
import           Data.Aeson
import           Data.Aeson.Lens                ( key
                                                , values
                                                , _String
                                                )
import           Data.Aeson.Types               ( parse )
import qualified Data.ByteString.UTF8          as BLU
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Either                    ( fromRight )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Read                 ( rational )
import           Debug.Trace                    ( trace )
import           GHC.Generics                   ( Generic )
import           Network.Wreq
import           SweetSpot.AppM                 ( AppConfig(..)
                                                , AppCtx(..)
                                                , AppM
                                                )
import           SweetSpot.Data.Api             ( Image(..)
                                                , Product(..)
                                                , Variant(..)
                                                )
import           SweetSpot.Data.Common

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

data ShopifyReq
  = ShopifyGet String
  | ShopifyPost String Value


data ExchangeBody = ExchangeBody
  { client_id :: Text
  , client_secret :: Text
  , code :: Text
  } deriving (Generic)

instance ToJSON ExchangeBody

toVariant :: StoreVariant -> Variant
toVariant v = Variant
        { _vId        = Svid . pack . show $ id (v :: StoreVariant)
        , _vProductId = Pid . pack . show . product_id $ v
        , _vTitle     = title (v :: StoreVariant)
        , _vSku       = Sku . sku $ v
        , _vPrice     = Price $ fst $ fromRight (0, "") (rational $ price v)
        }

toProduct :: StoreProduct -> Product
toProduct p = Product { _pId       = Pid . pack . show $ id (p :: StoreProduct)
                      , _pTitle    = title (p :: StoreProduct)
                      , _pVariants = toVariant <$> variants p
                      , _pImage    = toImage . image $ p
                      }
        where toImage i = Image { _iSrc = src i }

class Monad m => MonadShopify m where
  mkRequest :: ShopifyReq -> m (Response ByteString)
  fetchProducts :: m (Maybe [Product])
  fetchProductJson :: Pid -> m Value
  createProduct :: Value -> m (Result Product)
  exchangeAccessToken :: Text -> m Text

instance MonadShopify AppM where

        mkRequest r = do
                apiRoot <- asks (shopifyApiRoot . _getConfig)
                opts    <- getOpts
                liftIO $ case r of
                        ShopifyGet path -> getWith opts $ apiRoot <> path
                        ShopifyPost path payload ->
                                postWith opts (apiRoot <> path) payload

        fetchProducts = do
                r <-
                        mkRequest $ ShopifyGet
                                "/products.json?fields=id,title,variants,image"
                json <- asValue r
                let
                        result =
                                (json ^?! responseBody . key "products")
                                        ^.. values
                                        &   traverse (parse parseJSON)
                return $ case result of
                        Success ps -> Just (fmap toProduct ps)
                        Error   e  -> trace e Nothing

        fetchProductJson (Pid pid) = do
                r <- mkRequest
                        $ ShopifyGet ("/products/" <> unpack pid <> ".json")
                json <- asJSON r
                return $ json ^. responseBody

        createProduct v = do
                r    <- mkRequest $ ShopifyPost "/products.json" v
                json <- asValue r
                return
                        $   json
                        ^?! responseBody
                        .   key "product"
                        &   parse parseJSON
                        &   fmap toProduct

        exchangeAccessToken code = do
                config <- asks _getConfig
                let     accessTokenRoute = shopifyAccessTokenEndpoint config
                        body             = toJSON $ ExchangeBody
                                { client_id     = pack $ shopifyClientId config
                                , client_secret =
                                        pack $ shopifyClientSecret config
                                , code          = code
                                }

                r    <- mkRequest $ ShopifyPost accessTokenRoute body
                json <- asValue r
                return $ json ^?! responseBody . key "access_token" . _String

getOpts :: (MonadReader AppCtx m) => m Network.Wreq.Options
getOpts = do
        oauthToken <- asks (shopifyOAuthAccessToken . _getConfig)
        let token = BLU.fromString oauthToken
        return
                $  defaults
                &  header "X-Shopify-Access-Token"
                .~ [token]
                &  header "Accept"
                .~ ["application/json"]
                &  header "Content-Type"
                .~ ["application/json"]
