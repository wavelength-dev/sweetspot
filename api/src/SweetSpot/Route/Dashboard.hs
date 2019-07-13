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
import Data.Aeson.Lens (_String, key, nth)
import qualified Data.Text as T
import Prelude hiding (id)
import Servant
import SweetSpot.AppM (AppCtx(..), AppM)
import SweetSpot.Calc (enhanceDBStats)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database
  ( createExperiment
  , getExperimentBuckets
  , getCampaignStats
  )
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr)
import SweetSpot.ShopifyClient (createProduct, fetchProduct, fetchProducts)

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
  json <- fetchProduct $ ce ^. ceProductId
  let priceLens = key "product" . key "variants" . nth 0 . key "price" . _String
      textPrice = T.pack . show $ ce ^. cePrice
      withNewPrice = priceLens .~ textPrice $ json
  maybeNewProduct <- createProduct withNewPrice
  case maybeNewProduct of
    Just newProduct -> do
      let variant = newProduct ^?! pVariants . element 0
          sku = variant ^. vSku
          svid = variant ^. vId
          price = ce ^. cePrice
          name = ce ^. ceName
          campaignId = ce ^. ceCampaignId
      pool <- asks _getDbPool
      -- TODO: This is wrong just to please the compiler
      res <- liftIO $ createExperiment pool sku svid svid price campaignId name
      case res of
        Right _ -> do
          L.info "Created experiment"
          return OkResponse {message = "Created experiment"}
        Left err -> do
          L.error $ "Error creating experiment " <> err
          throwError internalServerErr
    Nothing -> throwError internalServerErr

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
