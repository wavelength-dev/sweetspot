{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Supple.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Applicative ((*>))
import Servant
import Supple.AppM (AppCtx(..), AppM)
import Supple.Database (getExperimentBuckets)
import Supple.ShopifyClient (createVariant, fetchProducts)
import Supple.Types

type ProductsRoute = "products" :> Get '[ JSON] [Product]

type ExperimentsRoute = "experiments" :> Get '[ JSON] [ExperimentBuckets]

type CreateVariantRoute
   = "variant" :> QueryParam "pid" Int :> ReqBody '[ JSON] CreateVariant :> Post '[ JSON] OkResponse

type DashboardAPI = CreateVariantRoute :<|> ProductsRoute :<|> ExperimentsRoute

createVariantHandler :: Maybe Int -> CreateVariant -> AppM OkResponse
createVariantHandler (Just pid) var =
  liftIO $ createVariant pid var *> (return OkResponse {message = "Created variant"})
createVariantHandler _ _ = throwError err500 {errBody = "Something went wrong"}

getProductsHandler :: AppM [Product]
getProductsHandler = liftIO fetchProducts

getExperimentsHandler :: AppM [ExperimentBuckets]
getExperimentsHandler = do
  dbconn <- asks _getDbConn
  liftIO $ getExperimentBuckets dbconn

dashboardHandler =
  createVariantHandler :<|> getProductsHandler :<|> getExperimentsHandler
