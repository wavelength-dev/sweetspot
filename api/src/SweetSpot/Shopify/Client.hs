module SweetSpot.Shopify.Client where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.Types (Result (..), parse, parseJSON)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO hiding ((^.), view)
import qualified RIO.Text as T
import Servant
import Servant.API (toUrlPiece)
import Servant.Client
import Servant.Links (safeLink)
import SweetSpot.AppM
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Install (InstallDB (..))
import SweetSpot.Env (Environment (..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Webhook
import SweetSpot.Shopify.Types

type ApiVersion = "2020-04"

type TokenExchangeRoute =
  "admin" :> "oauth" :> "access_token"
    :> ReqBody '[JSON] TokenExchangeReq
    :> Post '[JSON] TokenExchangeRes

type GetProductsRoute =
  "admin" :> "api" :> ApiVersion :> "products.json"
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Get '[JSON] Value

type GetProductJsonRoute =
  "admin" :> "api" :> ApiVersion :> "products"
    :> Capture "productId" Text
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Get '[JSON] Value

type CreateProductRoute =
  "admin" :> "api" :> ApiVersion :> "products.json"
    :> ReqBody '[JSON] Value
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Post '[JSON] Value

type DeleteProductRoute =
  "admin" :> "api" :> ApiVersion :> "products"
    :> Capture "productId" Text
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Delete '[JSON] NoContent

type RegisterWebhookRoute =
  "admin" :> "api" :> ApiVersion :> "webhooks.json"
    :> ReqBody '[JSON] CreateWebhookReq
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Post '[JSON] Value

type GetShopInfoRoute =
  "admin" :> "api" :> ApiVersion :> "shop.json"
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Get '[JSON] ShopInfoResponse

type CreateAppChargeRoute =
  "admin" :> "api" :> ApiVersion :> "recurring_application_charges.json"
    :> ReqBody '[JSON] CreateAppCharge
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Post '[JSON] CreateAppChargeRes

type GetAppChargeStatusRoute =
  "admin" :> "api" :> ApiVersion :> "recurring_application_charges"
    :> Capture "appChargeId" Text
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Get '[JSON] Value

type ActivateAppChargeRoute =
  "admin" :> "api" :> ApiVersion :> "recurring_application_charges"
    :> Capture "appChargeId" Text
    :> "activate.json"
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Post '[JSON] Value

type CreateScriptRoute =
  "admin" :> "api" :> ApiVersion :> "script_tags.json"
    :> ReqBody '[JSON] CreateScript
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Post '[JSON] Value

class Monad m => MonadShopify m where
  exchangeAccessToken :: ShopDomain -> Text -> m (Either Text Text)
  fetchProducts :: ShopDomain -> m (Either Text [ShopProduct])
  fetchProductJson :: ShopDomain -> Pid -> m (Either Text Value)
  createProduct :: ShopDomain -> Value -> m (Either Text ShopProduct)
  deleteProduct :: ShopDomain -> Pid -> m (Either Text ())
  registerWebhooks :: ShopDomain -> m (Either Text ())
  fetchShopInfo :: Text -> ShopDomain -> m (Either Text ShopInfo)
  createAppCharge :: ShopDomain -> m (Either Text CreateAppChargeRes)
  fetchAppChargeStatus :: ShopDomain -> Text -> m (Either Text AppChargeStatus)
  activateAppCharge :: ShopDomain -> Text -> m (Either Text AppChargeStatus)
  createScript :: ShopDomain -> m (Either Text ())

instance MonadShopify AppM where
  exchangeAccessToken domain code = do
    config <- asks (^. ctxConfig)
    clientEnv <- getClientEnv domain
    let exchangeTokenClient = client (Proxy :: Proxy TokenExchangeRoute)
        reqBody =
          TokenExchangeReq
            { client_id = config ^. configShopifyClientId,
              client_secret = config ^. configShopifyClientSecret,
              code = code
            }
    res <- liftIO $ runClientM (exchangeTokenClient reqBody) clientEnv
    return $ case res of
      Left err -> Left $ "Error exchanging auth token: " <> tshow err
      Right body -> Right $ access_token body

  fetchProducts domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let getProductsClient = client (Proxy :: Proxy GetProductsRoute)
      res <- liftIO $ runClientM (getProductsClient token) clientEnv
      return $ case res of
        Left err -> Left $ "Error getting products: " <> tshow err
        Right body -> do
          let result =
                body ^? key "products"
                  & fmap (traverse (parse parseJSON) . toListOf values)
          case result of
            Just (Success products) -> Right products
            Just (Error err) -> Left $ T.pack err
            Nothing -> Left "Missing key 'products' in products response"

  fetchProductJson domain productId =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let getProductJsonClient = client (Proxy :: Proxy GetProductJsonRoute)
      res <-
        liftIO $
          runClientM
            (getProductJsonClient (toQueryParam productId <> ".json") token)
            clientEnv
      return $ case res of
        Left err -> Left $ "Error fetching product json: " <> tshow err
        Right body -> Right body

  createProduct domain json =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let createProductClient = client (Proxy :: Proxy CreateProductRoute)
      res <- liftIO $ runClientM (createProductClient json token) clientEnv
      return $ case res of
        Left err -> Left $ "Error creating product: " <> tshow err
        Right body -> case parse parseJSON (body ^?! key "product") of
          Success product -> Right product
          Error err -> Left . T.pack $ err

  deleteProduct domain (Pid pid) =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let deleteProductClient = client (Proxy :: Proxy DeleteProductRoute)
      res <- liftIO $ runClientM (deleteProductClient (pid <> ".json") token) clientEnv
      return $ case res of
        Left err -> Left $ "Error deleting product: " <> tshow err
        Right _ -> Right ()

  fetchShopInfo token domain = do
    let fetchShopInfoClient = client (Proxy :: Proxy GetShopInfoRoute)
    clientEnv <- getClientEnv domain
    res <- liftIO $ runClientM (fetchShopInfoClient token) clientEnv
    return $ case res of
      Left err -> Left $ "Error fetching ShopInfo: " <> tshow err
      Right shopInfoRes -> Right $ _shopInfoResponseShop shopInfoRes

  registerWebhooks domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      apiRoot <- asks (view ctxConfig >>> view configSweetSpotApiRoot)
      let registerWebhook :: WebhookTopic -> AppM (Either ClientError Value)
          registerWebhook topic =
            liftIO $
              runClientM (createWebhookClient (getRequest apiRoot topic) token) clientEnv
      orderRes <- registerWebhook OrdersCreate
      uninstallRes <- registerWebhook AppUninstalled
      case partitionEithers [orderRes, uninstallRes] of
        ([], _) -> do
          L.info "Succesfully registered webhooks"
          pure $ Right ()
        (errs, _) ->
          map tshow errs
            & T.intercalate "\n"
            & Left
            & pure
    where
      createWebhookClient = client (Proxy :: Proxy RegisterWebhookRoute)
      webhookAPI = Proxy :: Proxy WebhookAPI
      orderPath =
        toUrlPiece $
          safeLink webhookAPI (Proxy :: Proxy OrderRoute)
      appUninstalledPath =
        toUrlPiece $
          safeLink webhookAPI (Proxy :: Proxy AppUninstalledRoute)
      getRequest :: Text -> WebhookTopic -> CreateWebhookReq
      getRequest apiRoot topic =
        CreateWebhookReq $
          CreateWebhookData
            { topic = tshow topic,
              address =
                apiRoot
                  <> ( case topic of
                         OrdersCreate -> orderPath
                         AppUninstalled -> appUninstalledPath
                     ),
              format = "json"
            }

  createAppCharge domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      conf <- asks (^. ctxConfig)
      let env = conf ^. configEnvironment
          apiRoot = conf ^. configSweetSpotApiRoot
          body =
            CreateAppCharge
              { _createAppChargeName = "SweetSpot Price Optimization",
                _createAppChargePrice = Price 200,
                _createAppChargeReturnUrl =
                  apiRoot
                    <> "charge/activate?shop="
                    <> showText domain,
                _createAppChargeIsTest = env /= Prod
              }
          createAppChargeClient = client (Proxy :: Proxy CreateAppChargeRoute)
      res <- liftIO $ runClientM (createAppChargeClient body token) clientEnv
      return $ case res of
        Left err -> Left $ "Error fetching ShopInfo: " <> tshow err
        Right appChargeRes -> Right appChargeRes

  fetchAppChargeStatus domain chargeId =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let getAppChargeClient = client (Proxy :: Proxy GetAppChargeStatusRoute)
      res <-
        liftIO $
          runClientM
            (getAppChargeClient (chargeId <> ".json") token)
            clientEnv
      pure $ case res of
        Left err -> Left $ "Error fetching AppCharge: " <> tshow err
        Right body ->
          case parseAppChargeStatus body of
            Just status -> Right status
            Nothing -> Left "Unable to parse AppChargeStatus from GetAppChargeStatusRoute"

  activateAppCharge domain chargeId =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let c = client (Proxy :: Proxy ActivateAppChargeRoute)
      res <- liftIO $ runClientM (c chargeId token) clientEnv
      pure $ case res of
        Left err -> Left $ "Error activating AppCharge: " <> tshow err
        Right body ->
          case parseAppChargeStatus body of
            Just status -> Right status
            Nothing -> Left "Unable to parse AppChargeStatus from ActivateAppChargeRoute"

  createScript domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let c = client (Proxy :: Proxy CreateScriptRoute)
          body =
            CreateScript
              { _createScriptEvent = "onload",
                _createScriptSrc = "https://" <> showText domain <> "/apps/sweetspot/fulcrum/fulcrum.js"
              }
      res <- liftIO $ runClientM (c body token) clientEnv
      pure $ case res of
        Left err -> Left $ "Error creating script: " <> tshow err
        Right _ -> Right ()

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

parseAppChargeStatus :: Value -> Maybe AppChargeStatus
parseAppChargeStatus v =
  v ^? key "recurring_application_charge"
    . key "status"
    . _JSON
