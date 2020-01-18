{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module SweetSpot.Database.Queries.Dashboard
        ( DashboardDB(..)
        , InsertExperiment(..)
        )
where

import           Control.Lens
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres

import           SweetSpot.AppM                 ( AppM )
import           SweetSpot.Data.Api             ( UICampaign(..) )
import           SweetSpot.Data.Common

import           SweetSpot.Database.Schema
import           SweetSpot.Database.Queries.Util
                                                ( withConn )

data InsertExperiment = InsertExperiment
  { _insertExperimentSku :: !Sku
  , _insertExperimentSvid :: !Svid
  , _insertExperimentProductId :: !Pid
  , _insertExperimentPrice :: !Price
  , _insertExperimentShopDomain :: !ShopDomain
  , _insertExperimentCampaignId :: !CampaignId
  , _insertExperimentProductName :: !Text
  , _insertExperimentTreatment :: !Int
  }

makeLenses ''InsertExperiment

class Monad m => DashboardDB m where
  createExperiment :: InsertExperiment -> m ()
  getCampaigns :: ShopDomain -> m [UICampaign]
--   getCampaignStats :: CampaignId -> m DBCampaignStats
--   getDashboardExperiments :: m [Api.ExperimentBuckets]


instance DashboardDB AppM where
  createExperiment args = withConn $ \conn -> do
    let
      domain = args ^. insertExperimentShopDomain

    Just shopId <- runBeamPostgres conn
      $ runSelectReturningOne
      $ select $ do
        row <- filter_ ((==. val_ domain) . (^. shopDomain)) (all_ (db ^. shops))
        pure $ row ^. shopId

    [dbVariant] <- runBeamPostgres conn
      $ runInsertReturningList
      $ insert (db ^. productVariants)
      $ insertExpressions
          [ ProductVariant
              { _pvId = productVariant_
              , _pvShopId = val_ $ ShopKey shopId
              , _pvTitle = val_ $ args ^. insertExperimentProductName
              , _pvSku = val_ $ args ^. insertExperimentSku
              , _pvProductId = val_ $ args ^. insertExperimentProductId
              , _pvVariantId = val_ $ args ^. insertExperimentSvid
              , _pvPrice = val_ $ args ^. insertExperimentPrice
              , _pvCurrency = val_ "USD"
              }
          ]

    runBeamPostgres conn
      $ runInsert
      $ insert (db ^. treatments)
      $ insertExpressions
            [ Treatment
                { _trCmpId = val_ $ CampaignKey $ args ^. insertExperimentCampaignId
                , _trTreatment = val_ $ args ^. insertExperimentTreatment
                , _trProductVariantId = val_ $ PVariantKey $ dbVariant ^. pvId
                }
            ]


  getCampaigns domain = withConn $ \conn -> do
    cmps <- runBeamPostgres conn
      $ runSelectReturningList
      $ select
      $ do
        shop <- filter_ ((==. val_ domain) . (^. shopDomain)) $ all_ (db ^. shops)
        filter_ ((==. (shop ^. shopId)) . (^. cmpShopId)) $ all_ (db ^. campaigns)

    traverse (getTreatments conn) cmps

    where
      getTreatments conn cmp = do
        tuples <- runBeamPostgres conn
          $ runSelectReturningList
          $ select
          $ do
              t <- filter_
                ((==. val_ (_cmpId cmp)) . (^. trCmpId))
                $ all_ (db ^. treatments)
              v <- all_ (db ^. productVariants)
              guard_ (_trProductVariantId t `references_` v)
              pure (t, v)

        return UICampaign
          { _uiCampaign = cmp
          , _uiCampaignTreatments = map fst tuples
          , _uiCampaignProductVariants = map snd tuples
          }
