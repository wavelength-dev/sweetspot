module SweetSpot.Route.Dashboard
  ( DashboardAPI,
    dashboardHandler,
  )
where

import Control.Lens hiding (Strict)
import Data.Aeson (Result (..), Value (..), parseJSON)
import Data.Aeson.Lens
import Data.Aeson.Types (parse)
import RIO hiding ((^.), to, view)
import qualified RIO.HashMap as HM
import qualified RIO.List as L
import RIO.Partial (fromJust)
import Servant
import SweetSpot.AppM (AppM (..), ServerM)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Data.Mapping (fromShopProduct)
import SweetSpot.Database.Queries.Dashboard (DashboardDB (..), InsertExperiment (..))
import qualified SweetSpot.Logger as Log
import SweetSpot.Route.Util (internalServerErr, unauthorizedErr)
import SweetSpot.Shopify.Client (MonadShopify (..))
import SweetSpot.Shopify.Types
import SweetSpot.Util (scientificToIntText)

type ProductsRoute =
  "products"
    :> QueryParam' '[Required, Strict] "session" SessionId
    :> QueryParam "page_info" PageInfo
    :> Get '[JSON] ProductsResponse

type CampaignRoute =
  "campaigns"
    :> QueryParam' '[Required, Strict] "session" SessionId
    :> Get '[JSON] [UICampaign]

type CreateCampaignRoute =
  "campaigns"
    :> QueryParam' '[Required, Strict] "session" SessionId
    :> ReqBody '[JSON] CreateCampaign
    :> Post '[JSON] OkResponse

type StopCampaignRoute =
  "campaigns" :> Capture "campaignId" CampaignId :> "stop"
    :> QueryParam' '[Required, Strict] "session" SessionId
    :> Post '[JSON] OkResponse

type DashboardAPI =
  "dashboard"
    :> (ProductsRoute :<|> CampaignRoute :<|> CreateCampaignRoute :<|> StopCampaignRoute)

getProductsHandler :: SessionId -> Maybe PageInfo -> ServerM ProductsResponse
getProductsHandler id mPageInfo = runAppM $ do
  mDomain <- validateSessionId id
  case mDomain of
    Just domain -> do
      mProducts <- fetchProducts domain mPageInfo
      case mProducts of
        Right (pagination, products) -> do
          moneyFormat <- unsafeGetShopMoneyFormat domain
          return $
            products
              & L.filter
                ( view shopProductType
                    >>> ( \case
                            Nothing -> True
                            Just productType -> productType /= "sweetspot-variant"
                        )
                )
              & mapMaybe (fromShopProduct moneyFormat)
              & \ps ->
                ProductsResponse
                  { _pagination = pagination,
                    _products = ps
                  }
        Left err -> do
          Log.error err
          throwError internalServerErr
    Nothing -> throwError $ err400 {errBody = "Bad sessionId, no domain found"}

getCampaignsHandler :: SessionId -> ServerM [UICampaign]
getCampaignsHandler id = runAppM $ do
  mDomain <- validateSessionId id
  case mDomain of
    Just domain -> getCampaigns domain
    Nothing -> throwError $ err400 {errBody = "Bad sessionId, no domain found"}

createCampaignHandler :: SessionId -> CreateCampaign -> ServerM OkResponse
createCampaignHandler id cc = runAppM $ do
  mDomain <- validateSessionId id
  case mDomain of
    Just domain -> do
      newCmpId <- createCampaign domain cc
      traverse_ (createCampaignExperiment domain newCmpId) (cc ^. createCampaignExperiments)
      return OkResponse {message = "Campaign created"}
    Nothing -> throwError $ err400 {errBody = "Bad sessionId, no domain found"}

createCampaignExperiment :: ShopDomain -> CampaignId -> CreateExperiment -> AppM ()
createCampaignExperiment domain cmpId ce = do
  mJson <- fetchProductJson domain (ce ^. createExperimentProductId)
  case mJson of
    Left err -> do
      Log.error $ "Failed to fetch product json " <> err
      throwError internalServerErr
    Right json -> do
      let mControlProduct = json ^?! key "product" . to (parse parseJSON) :: Result ShopProductWithSkus
      case mControlProduct of
        Error str -> do
          Log.error $ "Failed to parse control product " <> tshow str
          throwError $ err400 {errBody = "Failed to parse control product"}
        Success controlProduct -> do
          let variantPrices = ce ^. createExperimentVariants
              withNewPrice =
                json
                  & key "product" . key "variants" . values . _Object
                  %~ ( \variant ->
                         let variantId = variant ^?! at "id" . _Just . _Number . to scientificToIntText
                             newPrice =
                               L.find (view createVariantSvid >>> (\(Svid txt) -> txt) >>> (==) variantId) variantPrices
                                 & fromJust
                                 & view createVariantPrice
                                 & showText
                                 & String
                          in HM.insert "price" newPrice variant
                     )
                  & key "product" . key "handle" . _String <>~ "-ssv"
                  & key "product" . key "product_type" . _String .~ "sweetspot-variant"
                  & key "product" . key "images" . values . key "variant_ids" .~ Null
                  & key "product" . key "variants" . values . key "image_id" .~ Null
          mNewProduct <- createProduct domain withNewPrice
          case mNewProduct of
            Right newProduct -> do
              let controlVariant = controlProduct ^?! shopProductWithSkusVariants . ix 0
                  testVariant = newProduct ^?! shopProductWithSkusVariants . ix 0
                  controlPrice = controlVariant ^. shopVariantWithSkuPrice
                  testPrice = testVariant ^. shopVariantWithSkuPrice
                  createDbExperiment :: Int -> ShopVariantWithSku -> AppM ()
                  createDbExperiment treatment variant = createExperiment args
                    where
                      args =
                        InsertExperiment
                          { _insertExperimentSku = variant ^. shopVariantWithSkuSku,
                            _insertExperimentSvid = variant ^. shopVariantWithSkuId,
                            _insertExperimentProductId = variant ^. shopVariantWithSkuProductId,
                            _insertExperimentPrice = variant ^. shopVariantWithSkuPrice,
                            _insertExperimentShopDomain = domain,
                            _insertExperimentCampaignId = cmpId,
                            _insertExperimentProductName = controlProduct ^. shopProductWithSkusTitle,
                            _insertExperimentTreatment = treatment
                          }
              traverseOf_ (shopProductWithSkusVariants . traversed) (createDbExperiment 0) controlProduct
              traverseOf_ (shopProductWithSkusVariants . traversed) (createDbExperiment 1) newProduct
              Log.info "Created experiment(s)"
              return ()
            Left err -> do
              Log.error $ "Failed to create test product " <> err
              return ()

stopCampaignHandler :: CampaignId -> SessionId -> ServerM OkResponse
stopCampaignHandler campaignId sessionId = runAppM $ do
  authorized <- campaignBelongsToShop sessionId campaignId
  shopDomain <- validateSessionId sessionId
  Log.info $ "Stop campaign " <> tshow authorized <> " " <> tshow shopDomain
  case (authorized, shopDomain) of
    (True, Just domain) ->
      stopCampaign campaignId
        >> clearCampaignCache campaignId
        >> getTestVariantIds campaignId
        >>= traverse_ (deleteProduct domain)
        >> pure OkResponse {message = "Campaign stopped"}
    _ -> throwError unauthorizedErr

dashboardHandler =
  getProductsHandler
    :<|> getCampaignsHandler
    :<|> createCampaignHandler
    :<|> stopCampaignHandler
