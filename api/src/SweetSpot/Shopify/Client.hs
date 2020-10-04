{-# LANGUAGE TypeApplications #-}

module SweetSpot.Shopify.Client where

import Control.Lens hiding (Strict)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (ToJSON (..), Value)
import Data.Aeson.Lens
import Data.Aeson.Types (Result (..), parse, parseJSON)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import RIO hiding ((^.), to, view)
import qualified RIO.Text as T
import qualified RIO.Vector as V
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
import SweetSpot.Route.Webhook
import SweetSpot.Shopify.Pagination
import SweetSpot.Shopify.Types
import SweetSpot.Util (scientificToIntText)

type ApiVersion = "2020-04"

type TokenExchangeRoute =
  "admin" :> "oauth" :> "access_token"
    :> ReqBody '[JSON] TokenExchangeReq
    :> Post '[JSON] TokenExchangeRes

type GetProductsRoute =
  "admin" :> "api" :> ApiVersion :> "products.json"
    :> QueryParam "page_info" PageInfo
    :> QueryParam' '[Required, Strict] "limit" Int
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Get '[JSON] (Headers '[Header "Link" LinkHeader] Value)

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

type GetSmartCollectionsRoute =
  "admin" :> "api" :> ApiVersion :> "smart_collections.json"
    :> QueryParam' '[Required, Strict] "limit" Int
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Get '[JSON] Value

type UpdateSmartCollectionRoute =
  "admin" :> "api" :> ApiVersion :> "smart_collections"
    :> Capture "smartCollectionId" Text
    :> ReqBody '[JSON] SmartCollectionRuleUpdate
    :> Header' '[Required] "X-Shopify-Access-Token" Text
    :> Put '[JSON] Value

resultsPerPage :: Int
resultsPerPage = 250

class Monad m => MonadShopify m where
  exchangeAccessToken :: ShopDomain -> Text -> m (Either Text Text)
  fetchProducts :: ShopDomain -> Maybe PageInfo -> m (Either Text (Pagination, [ShopProduct]))
  fetchProductJson :: ShopDomain -> Pid -> m (Either Text Value)
  createProduct :: ShopDomain -> Value -> m (Either Text ShopProductWithSkus)
  deleteProduct :: ShopDomain -> Pid -> m (Either Text ())
  registerWebhooks :: ShopDomain -> m (Either Text ())
  fetchShopInfo :: Text -> ShopDomain -> m (Either Text ShopInfo)
  createAppCharge :: ShopDomain -> m (Either Text CreateAppChargeRes)
  fetchAppChargeStatus :: ShopDomain -> Text -> m (Either Text AppChargeStatus)
  activateAppCharge :: ShopDomain -> Text -> m (Either Text AppChargeStatus)
  createScript :: ShopDomain -> m (Either Text ())
  hideVariants :: ShopDomain -> m (Either Text ())

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

  fetchProducts domain mPageInfo =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let getProductsClient = client (Proxy :: Proxy GetProductsRoute)
      response <- liftIO $ runClientM (getProductsClient mPageInfo resultsPerPage token) clientEnv
      pure $ case response of
        Left err -> Left $ "Error getting products: " <> tshow err
        Right result ->
          let link = lookupResponseHeader result :: ResponseHeader "Link" LinkHeader
              (Headers body _) = result
              eProducts = parseProducts body
           in case (link, eProducts) of
                (Header h, Right products) -> Right (pagination, products)
                  where
                    pagination = parseLinkHeader h
                (_, Left err) -> Left err
                _ -> Left "Unable to parse link header"

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
      uninstallRes <- registerWebhook AppUninstalled
      case uninstallRes of
        Right _ -> do
          L.info "Succesfully registered webhooks"
          pure $ Right ()
        Left err -> pure $ Left $ tshow err
    where
      createWebhookClient = client (Proxy :: Proxy RegisterWebhookRoute)
      webhookAPI = Proxy :: Proxy WebhookAPI
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
        Left err -> Left $ "Error creating AppCharge: " <> tshow err
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
        Right body -> case parseAppChargeStatus body of
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

  hideVariants domain =
    withClientEnvAndToken domain $ \clientEnv token -> do
      let sweetspotRule = toJSON $ SmartCollectionRule "type" "not_equals" "sweetspot-variant"
          getCollections = client (Proxy :: Proxy GetSmartCollectionsRoute)
      res <- liftIO $ runClientM (getCollections resultsPerPage token) clientEnv
      case res of
        Left err -> pure $ Left $ "Error fetching SmartCollections: " <> tshow err
        Right body -> do
          let updateCollectionClient = client (Proxy :: Proxy UpdateSmartCollectionRoute)
              withUpdatedRules = body & key "smart_collections" . values . key "rules" . _Array %~ (flip V.snoc sweetspotRule)
              updateCollection value = liftIO $ runClientM (updateCollectionClient (id <> ".json") payload token) clientEnv
                where
                  id = value ^?! key "id" . _Number . to scientificToIntText
                  payload = value ^?! key "rules" . _JSON @Value @[SmartCollectionRule] & SmartCollectionRuleUpdate
              updates = withUpdatedRules ^?! key "smart_collections" . _Array
          results <- traverse updateCollection updates
          case partitionEithers (V.toList results) of
            (errs, _) -> do
              unless (null errs) (L.warn $ "Errors while hiding variants: " <> tshow errs)
              pure $ Right ()

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

recursivelyFetchProducts ::
  (Client ClientM GetProductsRoute) ->
  ClientEnv ->
  Text ->
  Maybe PageInfo ->
  [ShopProduct] ->
  AppM (Either Text [ShopProduct])
recursivelyFetchProducts client clientEnv token mPageInfo accumProducts = do
  res <- liftIO $ runClientM (client mPageInfo resultsPerPage token) clientEnv
  case res of
    Left err -> Left ("Error getting products: " <> tshow err) & pure
    Right res -> do
      let link = lookupResponseHeader res :: ResponseHeader "Link" LinkHeader
          (Headers body _) = res
          eProducts = parseProducts body
      case (link, eProducts) of
        (Header h, Right newProducts) -> do
          let pagination = parseLinkHeader h
          case pagination ^. paginationNext of
            Just pageInfo -> recursivelyFetchProducts client clientEnv token (Just pageInfo) (accumProducts <> newProducts)
            Nothing -> Right (accumProducts <> newProducts) & pure
        (_, Right newProducts) -> Right (accumProducts <> newProducts) & pure
        (_, Left err) -> Left err & pure
  where
    parseProducts body =
      let result =
            body ^? key "products"
              & fmap (traverse (parse parseJSON) . toListOf values)
       in case result of
            Just (Success products) -> Right products
            Just (Error err) -> Left $ T.pack err
            Nothing -> Left "Missing key 'products' in products response"

parseProducts :: Value -> Either Text [ShopProduct]
parseProducts body =
  let result =
        body ^? key "products"
          & fmap (traverse (parse parseJSON) . toListOf values)
   in case result of
        Just (Success products) -> Right products
        Just (Error err) -> Left $ T.pack err
        Nothing -> Left "Missing key 'products' in products response"
