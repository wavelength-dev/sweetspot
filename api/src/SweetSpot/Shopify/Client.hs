module SweetSpot.Shopify.Client where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.Types (Result (..), parse)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO hiding ((^.))
import qualified RIO.Text as T
import Servant
import Servant.API (toUrlPiece)
import Servant.Client
import Servant.Links (safeLink)
import SweetSpot.AppM
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Install (InstallDB (..))
import SweetSpot.Env (Environment (..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Webhook (OrderRoute, WebhookAPI)
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

instance MonadShopify AppM where
  exchangeAccessToken domain code =
    withClientEnvAndToken domain $ \clientEnv _ -> do
      config <- asks (^. ctxConfig)
      let exchangeTokenClient = client (Proxy :: Proxy TokenExchangeRoute)
          reqBody =
            TokenExchangeReq
              { client_id = config ^. configShopifyClientId,
                client_secret = config ^. configShopifyClientSecret,
                code = code
              }
      res <- liftIO $ runClientM (exchangeTokenClient reqBody) clientEnv
      return $ case res of
        Left err -> Left $ "Error exchanging auth token: " <> errToText err
        Right body -> Right $ access_token body

  fetchProducts domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let getProductsClient = client (Proxy :: Proxy GetProductsRoute)
      res <- liftIO $ runClientM (getProductsClient (Just token)) clientEnv
      return $ case res of
        Left err -> Left $ "Error getting products: " <> errToText err
        Right body -> do
          let result =
                body ^? key "products"
                  & fmap (traverse (parse parseShopJSON) . toListOf values)
          case result of
            Just (Success products) -> Right products
            Just (Error err) -> Left $ T.pack err
            Nothing -> Left "Missing key 'products' in products response"

  fetchProductJson domain productId =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let getProductJsonClient = client (Proxy :: Proxy GetProductJsonRoute)
      res <- liftIO $ runClientM (getProductJsonClient productId (Just token)) clientEnv
      return $ case res of
        Left err -> Left $ "Error fetching product json: " <> errToText err
        Right body -> Right body

  createProduct domain json =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let createProductClient = client (Proxy :: Proxy CreateProductRoute)
      res <- liftIO $ runClientM (createProductClient json (Just token)) clientEnv
      return $ case res of
        Left err -> Left $ "Error creating product: " <> errToText err
        Right body -> case parse parseShopJSON (body ^?! key "product") of
          Success product -> Right product
          Error err -> Left . T.pack $ err

  createCheckoutWebhook domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      res <- liftIO $ runClientM (createCheckoutWebhookClient payload (Just token)) clientEnv
      case res of
        Left err -> pure . Left $ "Error creating webhook: " <> errToText err
        Right _ -> do
          L.info $ "Created checkout webhook for " <> showText domain
          pure $ Right ()
    where
      createCheckoutWebhookClient = client (Proxy :: Proxy CreateWebhookRoute)
      hookPath =
        toUrlPiece $
          safeLink (Proxy :: Proxy WebhookAPI) (Proxy :: Proxy OrderRoute)
      payload =
        CreateWebhookReq $
          CreateWebhookData
            { topic = "orders/create",
              address = "https://app-staging.sweetspot.dev/api/" <> hookPath,
              format = "json"
            }

withClientEnvAndToken ::
  ShopDomain ->
  (ClientEnv -> Text -> AppM (Either Text a)) ->
  AppM (Either Text a)
withClientEnvAndToken domain f = do
  clientEnv <- getClientEnv domain
  mToken <- getOAuthToken domain
  case mToken of
    Just token -> f clientEnv token
    Nothing ->
      return . Left $ "Could not find OAuth token for shop domain " <> showText domain

getClientEnv :: ShopDomain -> AppM ClientEnv
getClientEnv domain = do
  manager <- liftIO $ newManager tlsManagerSettings
  config <- asks (^. ctxConfig)
  let baseUrl = case config ^. configEnvironment of
        TestBusiness -> testBaseUrl
        _ ->
          BaseUrl
            { baseUrlScheme = Https,
              baseUrlHost = show domain,
              baseUrlPort = 443,
              baseUrlPath = ""
            }
  return $ mkClientEnv manager baseUrl

testBaseUrl :: BaseUrl
testBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 9999,
      baseUrlPath = ""
    }

errToText :: ClientError -> Text
errToText = T.pack . show
