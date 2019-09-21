{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  ) where

import Control.Lens
import Control.Monad (unless)
import Data.Aeson (Result(..), parseJSON, Value(..))
import Data.Aeson.Lens (_String, key, values)
import Data.Aeson.Types (parse)
import qualified Data.List as L
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Prelude hiding (id)
import Servant
import SweetSpot.AppM (ServerM, AppM(..))
import SweetSpot.Calc (enhanceDBStats)
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable (InjectableDB(..))
import SweetSpot.Database.Queries.Dashboard (DashboardDB(..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr, badRequestErr)
import SweetSpot.ShopifyClient (MonadShopify(..), toProduct)

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type CreateExperimentRoute
   = "experiments" :> ReqBody '[ JSON] CreateExperiment :> Post '[ JSON] OkResponse

type CampaignStatsRoute
   = "campaigns" :> Capture "campaignId" T.Text :> "stats" :> Get '[ JSON] CampaignStats

type DashboardAPI
   = "dashboard" :> (ProductsRoute :<|> ExperimentsRoute :<|> CreateExperimentRoute :<|> CampaignStatsRoute)

getProductsHandler :: ServerM [Product]
getProductsHandler = runAppM $ do
  maybeProducts <- fetchProducts
  case maybeProducts of
    Just ps -> return ps
    Nothing -> throwError internalServerErr

getExperimentsHandler :: ServerM [ExperimentBuckets]
getExperimentsHandler = runAppM getDashboardExperiments

createExperimentHandler :: CreateExperiment -> ServerM OkResponse
createExperimentHandler ce = runAppM $ do
  isValidCampaign <- validateCampaign (ce ^. ceCampaignId)
  unless isValidCampaign (throwError badRequestErr)
  json <- fetchProductJson $ ce ^. ceProductId
  let
    contProduct = parse parseJSON $ json ^?! key "product"
    textPrice = T.pack . show $ ce ^. cePrice
    -- Assumes all variants have the same price
    withNewPrice =
      json
        & key "product" . key "variants" . values . key "price" . _String .~ textPrice
        & key "product" . key "handle" . _String <>~ "-ssv"
        & key "product" . key "product_type" . _String .~ "sweetspot-variant"
        & key "product" . key "images" . values . key "variant_ids" .~ Null
        & key "product" . key "variants" . values . key "image_id" .~ Null
        & key "product" . key "tags" .~ Null

  maybeNewProduct <- createProduct withNewPrice
  case (contProduct, maybeNewProduct) of
    (Success contProduct', Success newProduct) -> do
      let contProduct = toProduct contProduct'
          variant = newProduct ^?! pVariants . element 0
          testPrice = ce ^. cePrice
          title = contProduct ^. pTitle
          cmpId = ce ^. ceCampaignId
      res <- newProduct ^. pVariants
        & traverse (\v -> do
          let
            sku = v ^. vSku
            controlVariant = contProduct ^. pVariants ^.. traverse
              & L.find ((== sku) . (^. vSku))
              & fromJust
            contSvid = controlVariant ^. vId
            contPrice = controlVariant ^. vPrice
            testSvid = v ^. vId

          createExperiment (sku, contSvid, testSvid, contPrice, testPrice, cmpId, title))

      L.info "Created experiment(s)"
      return OkResponse { message = "Created experiment(s)"}

    (Error err, _) -> do
      L.error $ "Failed to parse control product " <> T.pack err
      throwError internalServerErr
    (_, Error err) -> do
      L.error $ "Failed to create new product" <> T.pack err
      throwError internalServerErr


getCampaignStatsHandler :: T.Text -> ServerM CampaignStats
getCampaignStatsHandler cmpId = runAppM $ do
  let cid = CampaignId cmpId
  isValid <- validateCampaign cid
  if isValid
    then do
      L.info ("Got experiment stats for campaignId: " <> cmpId)
      dbStats <- getCampaignStats cid
      enhanceDBStats dbStats
    else
      throwError err404

dashboardHandler =
  getProductsHandler :<|> getExperimentsHandler :<|> createExperimentHandler :<|> getCampaignStatsHandler
