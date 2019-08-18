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
import Data.Aeson (Result(..))
import Data.Aeson.Lens (_String, key, values)
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
import SweetSpot.Database
  ( getExperimentBuckets
  , getCampaignStats
  )
import SweetSpot.Database.Queries.Injectable (validateCampaign)
import SweetSpot.Database.Queries.Dashboard (createExperiment)
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr, badRequestErr)
import SweetSpot.ShopifyClient (createProduct, fetchProduct, fetchProducts, parseProduct)

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
  res <- liftIO $ getExperimentBuckets pool
  case res of
    Right exps -> return exps
    Left err -> do
      L.error $ "Error getting experiments " <> err
      throwError internalServerErr

createExperimentHandler :: CreateExperiment -> AppM OkResponse
createExperimentHandler ce = do
  pool <- asks _getDbPool
  pool' <- asks _getNewDbPool
  isValidCampaign <- liftIO . withResource pool'
    $ \conn -> validateCampaign conn (ce ^. ceCampaignId)
  unless isValidCampaign (throwError badRequestErr)
  json <- fetchProduct $ ce ^. ceProductId
  let
    contProduct = parseProduct $ json ^?! key "product"
    textPrice = T.pack . show $ ce ^. cePrice
    -- Assumes all variants have the same price
    withNewPrice =
      json
        & key "product" . key "variants" . values . key "price" . _String .~ textPrice
        & key "product" . key "handle" . _String <>~ "-ssv"

  maybeNewProduct <- createProduct withNewPrice
  case (contProduct, maybeNewProduct) of
    (Success contProduct, Success newProduct) -> do
      let variant = newProduct ^?! pVariants . element 0
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

          liftIO . withResource pool'
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
  res <- liftIO $ getCampaignStats pool (CampaignId cmpId)
  case res of
    Right dbStats ->
      L.info ("Got experiment stats for campaignId: " <> cid)
      >> enhanceDBStats dbStats
    Left err -> do
      L.error $ "Error getting experiment stats for campaignId: "
        <> cid <> " " <> err
      throwError err404 { errBody = "Could not find experiment" }
  where
    cid = T.pack $ show cmpId

dashboardHandler =
  getProductsHandler :<|> getExperimentsHandler :<|> createExperimentHandler :<|> getCampaignStatsHandler
