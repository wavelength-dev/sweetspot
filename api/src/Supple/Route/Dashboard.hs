{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Dashboard
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
import Supple.AppM (AppCtx(..), AppM)
import Supple.Data.Api
import Supple.Data.Common
import Supple.Data.Domain
import Supple.Database
  ( createExperiment
  , getExperimentBuckets
  , getExperimentStats
  )
import qualified Supple.Logger as L
import Supple.Route.Util (internalServerErr)
import Supple.ShopifyClient (createProduct, fetchProduct, fetchProducts)

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type CreateExperimentRoute
   = "experiments" :> ReqBody '[ JSON] CreateExperiment :> Post '[ JSON] OkResponse

type ExperimentStatsRoute
   = "experiments" :> Capture "expId" Int :> "stats" :> Get '[ JSON] ExperimentStats

type DashboardAPI
   = ProductsRoute :<|> ExperimentsRoute :<|> CreateExperimentRoute :<|> ExperimentStatsRoute

getProductsHandler :: AppM [Product]
getProductsHandler = do
  maybeProducts <- liftIO fetchProducts
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
  json <- liftIO $ fetchProduct $ ce ^. ceProductId
  let priceLens = key "product" . key "variants" . nth 0 . key "price" . _String
      textPrice = T.pack . show $ ce ^. cePrice
      withNewPrice = priceLens .~ textPrice $ json
  maybeNewProduct <- liftIO $ createProduct withNewPrice
  case maybeNewProduct of
    Just newProduct -> do
      let variant = newProduct ^?! pVariants . element 0
          sku = variant ^. vSku
          svid = variant ^. vId
          price = ce ^. cePrice
          name = ce ^. ceName
          campaignId = ce ^. ceCampaignId
      pool <- asks _getDbPool
      res <- liftIO $ createExperiment pool sku svid price campaignId name
      case res of
        Right _ -> do
          L.info "Created experiment"
          return OkResponse {message = "Created experiment"}
        Left err -> do
          L.error $ "Error creating experiment " <> err
          throwError internalServerErr
    Nothing -> throwError internalServerErr

getExperimentStatsHandler :: Int -> AppM ExperimentStats
getExperimentStatsHandler expId = do
  pool <- asks _getDbPool
  res <- liftIO $ getExperimentStats pool (ExpId expId)
  case res of
    Right dbStats ->
      L.info ("Got experiment stats for expId: " <> eid) >>
      (return $ enhanceDBStats dbStats)
    Left err -> do
      L.error $ "Error getting experiment stats for expId: " <> eid <> " " <>
        err
      throwError internalServerErr
  where
    eid = T.pack $ show expId

dashboardHandler =
  getProductsHandler :<|> getExperimentsHandler :<|> createExperimentHandler :<|>
  getExperimentStatsHandler
