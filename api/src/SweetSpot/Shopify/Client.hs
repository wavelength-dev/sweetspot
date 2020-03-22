{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Servant.API (toUrlPiece)
import Servant.Links (safeLink)
import Servant.Client

import SweetSpot.AppM
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Install (InstallDB(..))
import SweetSpot.Env (Environment(..))
import SweetSpot.Route.Webhook (CheckoutRoute, WebhookAPI)
import qualified SweetSpot.Logger as L
import SweetSpot.Shopify.Types

type ApiVersion = "2019-07"

type TokenExchangeRoute =
  "admin" :> "oauth" :> "access_token"
  :> ReqBody '[JSON] TokenExchangeReq
  :> Post '[JSON] TokenExchangeRes

type GetProductsRoute =
  "admin" :> "api" :> ApiVersion :> "products.json"
  :> Header "X-Shopify-Access-Token" Text
  :> Get '[JSON] Value

type GetProductJsonRoute =
  "admin" :> "api" :> ApiVersion :> "products"
  :> Capture "productId" Pid
  :> Header "X-Shopify-Access-Token" Text
  :> Get '[JSON] Value

type CreateProductRoute =
  "admin" :> "api" :> ApiVersion :> "products.json"
  :> ReqBody '[JSON] Value
  :> Header "X-Shopify-Access-Token" Text
  :> Post '[JSON] Value

type CreateWebhookRoute =
  "admin" :> "api" :> ApiVersion :> "webhooks.json"
  :> ReqBody '[JSON] CreateWebhookReq
  :> Header "X-Shopify-Access-Token" Text
  :> Post '[JSON] Value

class Monad m => MonadShopify m where
  exchangeAccessToken :: ShopDomain -> Text -> m (Either Text Text)
  fetchProducts :: ShopDomain -> m (Either Text [Product])
  fetchProductJson :: ShopDomain -> Pid -> m (Either Text Value)
  createProduct :: ShopDomain -> Value -> m (Either Text Product)
  createCheckoutWebhook :: ShopDomain -> m (Either Text ())

testBaseUrl :: BaseUrl
testBaseUrl = BaseUrl { baseUrlScheme = Http
                      , baseUrlHost = "localhost"
                      , baseUrlPort = 9999
                      , baseUrlPath = ""
                      }

getClientEnv :: ShopDomain -> AppM ClientEnv
getClientEnv domain = do
    manager <- liftIO $ newManager tlsManagerSettings
    config <- asks (^. ctxConfig)
    let
      baseUrl = case config ^. configEnvironment of
        TestBusiness -> testBaseUrl
        _ -> BaseUrl { baseUrlScheme = Https
                     , baseUrlHost = show domain
                     , baseUrlPort = 443
                     , baseUrlPath = ""
                     }

    return $ mkClientEnv manager baseUrl


instance MonadShopify AppM where
  exchangeAccessToken domain code = do
    config <- asks (^. ctxConfig)
    let
      exchangeTokenClient = client (Proxy :: Proxy TokenExchangeRoute)
      reqBody = TokenExchangeReq
        { client_id = config ^. configShopifyClientId
        , client_secret = config ^. configShopifyClientSecret
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

  createProduct domain json = do
    let
      createProductClient = client (Proxy :: Proxy CreateProductRoute)
    clientEnv <- getClientEnv domain
    mToken <- getOAuthToken domain
    case mToken of
      Just token -> do
        res <- liftIO $ runClientM (createProductClient json (Just token)) clientEnv
        return $ case res of
          Left err -> Left $ "Error creating product: " <> (T.pack . show $ err)
          Right body -> case parse parseShopJSON (body ^?! key "product") of
            Success product -> Right product
            Error err -> Left . T.pack . show $ err

      Nothing -> return $ Left "Could not find OAuth token for shop domain"

  createCheckoutWebhook domain = do
    clientEnv <- getClientEnv domain
    mToken <- getOAuthToken domain
    case mToken of
      Just token -> do
        res <- liftIO $ runClientM (createCheckoutWebhookClient payload (Just token)) clientEnv
        case res of
          Left err -> pure . Left $ "Error creating webhook: " <> (T.pack . show $ err)
          Right _ -> do
            L.info $ "Created checkout webhook for " <> showText domain
            pure $ Right ()
      Nothing -> return $ Left "Could not find OAuth token for shop domain"

    where
      createCheckoutWebhookClient = client (Proxy :: Proxy CreateWebhookRoute)
      hookPath = toUrlPiece
        $ safeLink (Proxy :: Proxy WebhookAPI) (Proxy :: Proxy CheckoutRoute)
      payload = CreateWebhookReq $ CreateWebhookData
        { topic = "checkouts/create"
        , address = "https://app-staging.sweetspot.dev/api/" <> hookPath
        , format = "json"
        }
