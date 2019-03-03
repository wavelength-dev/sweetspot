{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  ) where

import Control.Lens ((.~), (^.), (^?!), element)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson.Lens (_String, key, nth)
import qualified Data.Text as T
import Prelude hiding (id)
import Servant
import Supple.AppM (AppCtx(..), AppM)
import Supple.Data.Common ()
import Supple.Data.Api
import Supple.Database (createExperiment, getExperimentBuckets)
import Supple.ShopifyClient (createProduct, fetchProduct, fetchProducts)

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type CreateExperimentRoute
   = "experiments" :> ReqBody '[ JSON] CreateExperiment :> Post '[ JSON] OkResponse

type DashboardAPI
   = ProductsRoute :<|> ExperimentsRoute :<|> CreateExperimentRoute

getProductsHandler :: AppM [Product]
getProductsHandler = do
  maybeProducts <- liftIO fetchProducts
  case maybeProducts of
    Just ps -> return ps
    Nothing -> throwError $ err500 {errBody = "Something went wrong"}

getExperimentsHandler :: AppM [ExperimentBuckets]
getExperimentsHandler = do
  dbconn <- asks _getDbConn
  liftIO $ getExperimentBuckets dbconn

createExperimentHandler :: CreateExperiment -> AppM OkResponse
createExperimentHandler ce = do
  json <- liftIO $ fetchProduct $ ce ^. ceProductId
  let priceLens = key "product" . key "variants" . nth 0 . key "price" . _String
      textPrice = T.pack . show $ ce ^. cePrice
      withNewPrice = priceLens .~ textPrice $ json

  maybeNewProduct <- liftIO $ createProduct withNewPrice

  case maybeNewProduct of
    Just newProduct -> do
      let
        variant = newProduct ^?! pVariants . element 0
        sku = variant ^. vSku
        svid = variant ^. vId
        price = ce ^. cePrice
        name = ce ^. ceName
        campaignId = ce ^. ceCampaignId

      dbconn <- asks _getDbConn
      liftIO $ createExperiment dbconn sku svid price campaignId name
      return OkResponse {message = "Created experiment"}

    Nothing -> throwError $ err500 {errBody = "Something went wrong"}

dashboardHandler =
  getProductsHandler :<|> getExperimentsHandler :<|> createExperimentHandler
