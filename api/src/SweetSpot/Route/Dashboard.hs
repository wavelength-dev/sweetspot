{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SweetSpot.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  ) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad (sequence)
import Data.Aeson (Value(..))
import Data.Aeson.Lens (_String, key, values)
import qualified Data.List as L
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import Data.Text.Read (rational)
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
  let
    textPrice = T.pack . show $ ce ^. cePrice
    variantsT = key "product" . key "variants" . values
    -- Assumes all variants have the same price
    withNewPrice = json & variantsT . key "price" . _String .~ textPrice
  maybeNewProduct <- createProduct withNewPrice
  case maybeNewProduct of
    Just newProduct -> do
      let variant = newProduct ^?! pVariants . element 0
          testPrice = ce ^. cePrice
          name = json ^. key "product" . key "title" . _String
          cmpId = ce ^. ceCampaignId
      pool <- asks _getDbPool
      -- I'm sorry for this
      res <- liftIO $ newProduct ^. pVariants
        & traverse (\v -> do
          let
            sku = v ^. vSku
            controlVariant = json ^?! key "product" . key "variants" ^.. values
              & L.find (\v -> Sku (v ^. key "sku" . _String) == sku)
              & fromJust

            contSvid = controlVariant ^?! key "id"
              & \case
                  Number n -> (Svid . fromJust . toBoundedInteger) n
                  _ -> undefined

            contPrice = controlVariant ^?! key "price"
              & \case
                   String s -> Price $ fst $ fromRight (0, "") (rational s)
                   _ -> undefined

            testSvid = v ^. vId

          createExperiment pool sku contSvid testSvid contPrice testPrice cmpId name)

      case sequence res of
        Right _ -> do
          L.info "Created experiment(s)"
          return OkResponse {message = "Created experiment(s)"}
        Left err -> do
          L.error $ "Error creating experiment(s) " <> err
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
