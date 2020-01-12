{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module SweetSpot.Shopify.Client where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.Types (parse, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant
import Servant.Client

import SweetSpot.AppM
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Install (InstallDB(..))
import SweetSpot.Env (Environment(..))
import SweetSpot.Shopify.Types

type TokenExchangeRoute =
  "admin" :> "oauth" :> "access_token"
  :> ReqBody '[JSON] TokenExchangeReq
  :> Post '[JSON] TokenExchangeRes

type GetProductsRoute =
  "admin" :> "api" :> "2019-07" :> "products.json"
  :> Header "X-Shopify-Access-Token" Text
  :> Get '[JSON] Value

type GetProductJsonRoute =
  "admin" :> "api" :> "2019-07" :> "products"
  :> Capture "productId" Pid
  :> Header "X-Shopify-Access-Token" Text
  :> Get '[JSON] Value

class Monad m => MonadShopify m where
  exchangeAccessToken :: ShopDomain -> Text -> m (Either Text Text)
  fetchProducts :: ShopDomain -> m (Either Text [Product])
  fetchProductJson :: ShopDomain -> Pid -> m (Either Text Value)

testBaseUrl :: BaseUrl
testBaseUrl = BaseUrl { baseUrlScheme = Http
                      , baseUrlHost = "localhost"
                      , baseUrlPort = 9999
                      , baseUrlPath = ""
                      }

getClientEnv :: ShopDomain -> AppM ClientEnv
getClientEnv domain = do
    manager <- liftIO $ newManager tlsManagerSettings
    config <- asks _getConfig
    let
      baseUrl = case environment config of
        TestBusiness -> testBaseUrl
        _ -> BaseUrl { baseUrlScheme = Https
                     , baseUrlHost = show domain
                     , baseUrlPort = 443
                     , baseUrlPath = ""
                     }

    return $ mkClientEnv manager baseUrl

instance MonadShopify AppM where
  exchangeAccessToken domain code = do
    config <- asks _getConfig
    let
      exchangeTokenClient = client (Proxy :: Proxy TokenExchangeRoute)
      reqBody = TokenExchangeReq
        { client_id = shopifyClientId config
        , client_secret = shopifyClientSecret config
        , code = code
        }
    clientEnv <- getClientEnv domain
    res <- liftIO $ runClientM (exchangeTokenClient reqBody) clientEnv
    return $ case res of
      Left err -> Left $ "Error exchanging auth token: " <> (T.pack . show $ err)
      Right body -> Right $ access_token body

  fetchProducts domain = do
    let
      getProductsClient = client (Proxy :: Proxy GetProductsRoute)
    clientEnv <- getClientEnv domain
    mToken <- getOAuthToken domain
    case mToken of
      Just token -> do
        res <- liftIO $ runClientM (getProductsClient (Just token)) clientEnv
        return $ case res of
          Left err -> Left $ "Error getting products: " <> (T.pack . show) err
          Right body -> do
            let
              result = body ^? key "products"
                & fmap (traverse (parse parseShopJSON) . toListOf values)
            case result of
              (Just (Success products)) -> Right products
              (Just (Error err)) -> Left $ T.pack err
              Nothing -> Left "Missing key 'products' in products response"

      Nothing -> return $ Left "Could not find OAuth token for shop domain"

  fetchProductJson domain productId = do
    let
      getProductJsonClient = client (Proxy :: Proxy GetProductJsonRoute)
    clientEnv <- getClientEnv domain
    mToken <- getOAuthToken domain
    case mToken of
      Just token -> do
        res <- liftIO $ runClientM (getProductJsonClient productId (Just token)) clientEnv
        return $ case res of
          Left err -> Left $ "Error fetching product json: " <> (T.pack . show $ err)
          Right body -> Right body
      Nothing -> return $ Left "Could not find OAuth token for shop domain"
