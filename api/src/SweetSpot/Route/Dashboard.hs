{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Route.Dashboard
  ( DashboardAPI
  , dashboardHandler
  )
where

import Control.Lens
import Control.Monad (unless)
import Data.Aeson (Result(..), Value(..))
import Data.Aeson.Lens (_String, key, values)
import Data.Aeson.Types (parse)
import qualified Data.Text as T
import Servant
import SweetSpot.AppM (ServerM, AppM(..))
import SweetSpot.Data.Api
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Injectable (InjectableDB(..))
import SweetSpot.Database.Queries.Dashboard (DashboardDB(..), InsertExperiment(..))
import qualified SweetSpot.Logger as L
import SweetSpot.Route.Util (internalServerErr, badRequestErr)
import SweetSpot.Shopify.Client (MonadShopify(..))
import SweetSpot.Shopify.Types (FromShopJSON(..))

type ProductsRoute = "products"
  :> QueryParam "session" SessionId
  :> Get '[JSON] [Product]

type CampaignRoute = "campaigns"
  :> QueryParam "session" SessionId
  :> Get '[JSON] [UICampaign]

type CreateExperimentRoute = "experiments"
  :> ReqBody '[JSON] CreateExperiment
  :> Post '[JSON] OkResponse

type DashboardAPI = "dashboard"
  :> (ProductsRoute :<|> CampaignRoute :<|> CreateExperimentRoute)

getProductsHandler :: Maybe SessionId -> ServerM [Product]
getProductsHandler (Just id) = runAppM $ do
  mDomain <- validateSessionId id
  case mDomain of
    Just domain -> do
      mProducts <- fetchProducts domain
      case mProducts of
        Right ps -> return ps
        Left err -> do
          L.error err
          throwError internalServerErr
    Nothing -> do
      L.error "Invalid sessionId in getProductsHandler"
      throwError badRequestErr
getProductsHandler Nothing = throwError badRequestErr

getCampaignsHandler :: Maybe SessionId -> ServerM [UICampaign]
getCampaignsHandler (Just id) = runAppM $ do
  mDomain <- validateSessionId id
  case mDomain of
    Just domain -> getCampaigns domain
    Nothing -> do
      L.error "Invalid sessionId in getCampaignsHandler"
      throwError badRequestErr
getCampaignsHandler Nothing = throwError badRequestErr

createExperimentHandler :: CreateExperiment -> ServerM OkResponse
createExperimentHandler ce = runAppM $ do
  isValidCampaign <- validateCampaign (ce ^. ceCampaignId)
  unless isValidCampaign (throwError badRequestErr)
  mJson <- fetchProductJson (ce ^. ceShopDomain) (ce ^. ceProductId)
  case mJson of
    Left err -> do
      L.error err
      throwError internalServerErr
    Right json -> do
      let
        mControlProduct = parse parseShopJSON $ json ^?! key "product"
        textPrice = showText $ ce ^. cePrice
        -- Assumes all variants have the same price
        withNewPrice =
          json
            & key "product" . key "variants" . values . key "price" . _String .~ textPrice
            & key "product" . key "handle" . _String <>~ "-ssv"
            & key "product" . key "product_type" . _String .~ "sweetspot-variant"
            & key "product" . key "images" . values . key "variant_ids" .~ Null
            & key "product" . key "variants" . values . key "image_id" .~ Null

      mNewProduct <- createProduct (ce ^. ceShopDomain) withNewPrice
      case (mControlProduct, mNewProduct) of
        (Success controlProduct, Right newProduct) -> do
          let
            controlVariant = controlProduct ^?! productVariants . ix 0
            testVariant = newProduct ^?! productVariants . ix 0
            controlPrice = controlVariant ^. variantPrice
            testPrice = testVariant ^. variantPrice

            createDbExperiment :: Int -> Price -> Variant -> AppM ()
            createDbExperiment treatment price variant = createExperiment args
              where
                args = InsertExperiment
                  { _insertExperimentSku = variant ^. variantSku
                  , _insertExperimentSvid = variant ^. variantId
                  , _insertExperimentProductId = variant ^. variantProductId
                  , _insertExperimentPrice = price
                  , _insertExperimentShopDomain = ce ^. ceShopDomain
                  , _insertExperimentCampaignId = ce ^. ceCampaignId
                  , _insertExperimentProductName = controlProduct ^. productTitle
                  , _insertExperimentTreatment = treatment
                  }

          traverseOf_ (productVariants . traversed) (createDbExperiment 0 testPrice) controlProduct
          traverseOf_ (productVariants . traversed) (createDbExperiment 1 controlPrice) newProduct

          L.info "Created experiment(s)"
          return OkResponse { message = "Created experiment(s)"}

        (Error err, _) -> do
          L.error $ "Failed to parse control product " <> T.pack err
          throwError internalServerErr

        (_, Left err) -> do
          L.error $ "Failed to create test product " <> err
          throwError internalServerErr

dashboardHandler =
       getProductsHandler
  :<|> getCampaignsHandler
  :<|> createExperimentHandler
