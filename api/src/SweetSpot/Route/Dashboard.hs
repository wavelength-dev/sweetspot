module SweetSpot.Route.Dashboard
  ( DashboardAPI,
    dashboardHandler,
  )
where

import Control.Lens hiding (Strict)
import Data.Aeson (Result (..), Value (..), parseJSON)
import Data.Aeson.Lens (_String, key, values)
import Data.Aeson.Types (parse)
import RIO hiding ((^.))
import qualified RIO.List as L
import qualified RIO.Text as T
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

type ProductsRoute =
  "products"
    :> QueryParam' '[Required, Strict] "session" SessionId
    :> Get '[JSON] [Product]

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

getProductsHandler :: SessionId -> ServerM [Product]
getProductsHandler id = runAppM $ do
  mDomain <- validateSessionId id
  case mDomain of
    Just domain -> do
      mProducts <- fetchProducts domain
      case mProducts of
        Right ps -> do
          moneyFormat <- unsafeGetShopMoneyFormat domain
          return $
            ps
              & L.filter ((/= "sweetspot-variant") . (^. shopProductType))
              & L.map (fromShopProduct moneyFormat)
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
      Log.error err
      throwError internalServerErr
    Right json -> do
      let mControlProduct = parse parseJSON $ json ^?! key "product"
          textPrice = showText $ ce ^. createExperimentPrice
          -- Assumes all variants have the same price
          withNewPrice =
            json
              & key "product" . key "variants" . values . key "price" . _String .~ textPrice
              & key "product" . key "handle" . _String <>~ "-ssv"
              & key "product" . key "product_type" . _String .~ "sweetspot-variant"
              & key "product" . key "images" . values . key "variant_ids" .~ Null
              & key "product" . key "variants" . values . key "image_id" .~ Null
      mNewProduct <- createProduct domain withNewPrice
      case (mControlProduct, mNewProduct) of
        (Success controlProduct, Right newProduct) -> do
          let controlVariant = controlProduct ^?! shopProductVariants . ix 0
              testVariant = newProduct ^?! shopProductVariants . ix 0
              controlPrice = controlVariant ^. shopVariantPrice
              testPrice = testVariant ^. shopVariantPrice
              createDbExperiment :: Int -> Price -> ShopVariant -> AppM ()
              createDbExperiment treatment price variant = createExperiment args
                where
                  args =
                    InsertExperiment
                      { _insertExperimentSku = variant ^. shopVariantSku,
                        _insertExperimentSvid = variant ^. shopVariantId,
                        _insertExperimentProductId = variant ^. shopVariantProductId,
                        _insertExperimentPrice = price,
                        _insertExperimentShopDomain = domain,
                        _insertExperimentCampaignId = cmpId,
                        _insertExperimentProductName = controlProduct ^. shopProductTitle,
                        _insertExperimentTreatment = treatment
                      }
          traverseOf_ (shopProductVariants . traversed) (createDbExperiment 0 controlPrice) controlProduct
          traverseOf_ (shopProductVariants . traversed) (createDbExperiment 1 testPrice) newProduct
          Log.info "Created experiment(s)"
          return ()
        (Error err, _) -> do
          Log.error $ "Failed to parse control product " <> T.pack err
          return ()
        (_, Left err) -> do
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
        >> getTestVariantIds campaignId
        >>= traverse_ (deleteProduct domain)
        >> pure OkResponse {message = "Campaign stopped"}
    _ -> throwError unauthorizedErr

dashboardHandler =
  getProductsHandler
    :<|> getCampaignsHandler
    :<|> createCampaignHandler
    :<|> stopCampaignHandler
