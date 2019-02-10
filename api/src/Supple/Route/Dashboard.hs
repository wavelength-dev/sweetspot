{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Supple.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  ) where
import Prelude hiding (id)
import Control.Applicative ((*>))
import Control.Lens ((.~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson.Lens (_String, key, nth)
import Data.Text (pack)
import Servant
import Supple.AppM (AppCtx(..), AppM)
import Supple.Data.Api (CreateExperiment(..), CreateVariant, OkResponse(..))
import Supple.Data.Database (ExperimentBuckets)
import Supple.Data.Shopify (Product(variants), Variant(id, sku))
import Supple.Database (getExperimentBuckets, createExperiment)
import Supple.ShopifyClient
  ( createProduct
  , createVariant
  , fetchProduct
  , fetchProducts
  )

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type CreateVariantRoute
   = "variant" :> QueryParam "pid" Int :> ReqBody '[ JSON] CreateVariant :> Post '[ JSON] OkResponse

type CreateExperimentRoute
   = "experiments" :> ReqBody '[ JSON] CreateExperiment :> Post '[ JSON] OkResponse

type DashboardAPI
   = CreateVariantRoute :<|> ProductsRoute :<|> ExperimentsRoute :<|> CreateExperimentRoute

createVariantHandler :: Maybe Int -> CreateVariant -> AppM OkResponse
createVariantHandler (Just pid) var =
  liftIO $
  createVariant pid var *> (return OkResponse {message = "Created variant"})
createVariantHandler _ _ = throwError err500 {errBody = "Something went wrong"}

getProductsHandler :: AppM [Product]
getProductsHandler = liftIO fetchProducts

getExperimentsHandler :: AppM [ExperimentBuckets]
getExperimentsHandler = do
  dbconn <- asks _getDbConn
  liftIO $ getExperimentBuckets dbconn

createExperimentHandler :: CreateExperiment -> AppM OkResponse
createExperimentHandler CreateExperiment {..} = do
  json <- liftIO $ fetchProduct productId
  let
    priceLens = key "product" . key "variants" . nth 0 . key "price" . _String
    textPrice = pack . show $ price
    withNewPrice = priceLens .~ textPrice $ json
  newProduct <- liftIO $ createProduct withNewPrice
  let
    variant = (variants newProduct) !! 0
    s = sku variant
    sv = id variant
  dbconn <- asks _getDbConn
  liftIO $ createExperiment dbconn s sv price name
  return OkResponse {message = "Created experiment"}

dashboardHandler =
  createVariantHandler :<|> getProductsHandler :<|> getExperimentsHandler :<|>
  createExperimentHandler
