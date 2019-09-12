{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  ) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad (unless)
import Data.Aeson (Result(..), parseJSON, Value(..))
import Data.Aeson.Lens (_String, key, values)
import Data.Aeson.Types (parse)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Pool (withResource)
import qualified Data.Text as T
import Prelude hiding (id)
import Servant
import SweetSpot.AppM (AppCtx(..), AppM)
import SweetSpot.Calc (enhanceDBStats)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable (validateCampaign)
import SweetSpot.Database.Queries.Dashboard (createExperiment, getCampaignStats, getDashboardExperiments)
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr, badRequestErr)
import SweetSpot.ShopifyClient (createProduct, fetchProduct, fetchProducts, toProduct)

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type CreateExperimentRoute
   = "experiments" :> ReqBody '[ JSON] CreateExperiment :> Post '[ JSON] OkResponse

type CampaignStatsRoute
   = "campaigns" :> Capture "campaignId" T.Text :> "stats" :> Get '[ JSON] CampaignStats

type DashboardAPI
   = "dashboard" :> (ProductsRoute :<|> ExperimentsRoute :<|> CreateExperimentRoute :<|> CampaignStatsRoute)

getProductsHandler :: AppM [Product]
getProductsHandler = do
  maybeProducts <- fetchProducts
  case maybeProducts of
    Just ps -> return ps
    Nothing -> throwError internalServerErr

getExperimentsHandler :: AppM [ExperimentBuckets]
getExperimentsHandler = do
  pool <- asks _getDbPool
  liftIO . withResource pool $ \conn -> getDashboardExperiments conn

createExperimentHandler :: CreateExperiment -> AppM OkResponse
createExperimentHandler ce = do
  pool <- asks _getDbPool
  isValidCampaign <- liftIO . withResource pool
    $ \conn -> validateCampaign conn (ce ^. ceCampaignId)
  unless isValidCampaign (throwError badRequestErr)
  json <- fetchProduct $ ce ^. ceProductId
  let
    contProduct = parse parseJSON $ json ^?! key "product"
    textPrice = T.pack . show $ ce ^. cePrice
    -- Assumes all variants have the same price
    withNewPrice =
      json
        & key "product" . key "variants" . values . key "price" . _String .~ textPrice
        & key "product" . key "variants" . values . key "image_id" .~ Null
        & key "product" . key "handle" . _String <>~ "-ssv"
        & key "product" . key "product_type" . _String .~ "sweetspot-variant"

  maybeNewProduct <- createProduct withNewPrice
  case (contProduct, maybeNewProduct) of
    (Success contProduct', Success newProduct) -> do
      let contProduct = toProduct contProduct'
          variant = newProduct ^?! pVariants . element 0
          testPrice = ce ^. cePrice
          title = contProduct ^. pTitle
          cmpId = ce ^. ceCampaignId
      res <- liftIO $ newProduct ^. pVariants
        & traverse (\v -> do
          let
            sku = v ^. vSku
            controlVariant = contProduct ^. pVariants ^.. traverse
              & L.find ((== sku) . (^. vSku))
              & fromJust
            contSvid = controlVariant ^. vId
            contPrice = controlVariant ^. vPrice
            testSvid = v ^. vId

          liftIO . withResource pool
            $ \conn -> createExperiment conn (sku, contSvid, testSvid, contPrice, testPrice, cmpId, title))

      L.info "Created experiment(s)"
      return OkResponse { message = "Created experiment(s)"}

    (Error err, _) -> do
      L.error $ "Failed to parse control product " <> T.pack err
      throwError internalServerErr
    (_, Error err) -> do
      L.error $ "Failed to create new product" <> T.pack err
      throwError internalServerErr


getCampaignStatsHandler :: T.Text -> AppM CampaignStats
getCampaignStatsHandler cmpId = do
  pool <- asks _getDbPool
  let cid = CampaignId cmpId
  isValid <- liftIO . withResource pool $ \conn -> validateCampaign conn cid
  if isValid
    then do
      L.info ("Got experiment stats for campaignId: " <> cmpId)
      dbStats <- liftIO . withResource pool  $ \conn -> getCampaignStats conn cid
      enhanceDBStats dbStats
    else
      throwError err404

dashboardHandler =
  getProductsHandler :<|> getExperimentsHandler :<|> createExperimentHandler :<|> getCampaignStatsHandler
