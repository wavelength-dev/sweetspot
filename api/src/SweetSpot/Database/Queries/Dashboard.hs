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

import qualified Data.Map.Strict               as M
import           SweetSpot.AppM                 ( AppM )
import           SweetSpot.Data.Api hiding (productVariants)
import           SweetSpot.Data.Common

import           SweetSpot.Database.Schema
import           SweetSpot.Database.Queries.Util
                                                ( withConn
                                                , matchShop
                                                )

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
  getCampaignStats :: ShopDomain -> CampaignId -> m CampaignStats

instance DashboardDB AppM where
  createExperiment args = withConn $ \conn -> do
    let
      domain = args ^. insertExperimentShopDomain

    Just shopId <- runBeamPostgres conn
      $ runSelectReturningOne
      $ select $ do
        row <- matchShop domain
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
        shop <- matchShop domain
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
          { _uiCampaignId = cmp ^. cmpId
          , _uiCampaignStart = cmp ^. cmpStart
          , _uiCampaignEnd = cmp ^. cmpEnd
          , _uiCampaignTreatments = map mkUiTreatment tuples
          }

      mkUiTreatment (t, v) =
        UITreatment
          { _uiTreatmentSvid = v ^. pvVariantId
          , _uiTreatmentTitle = v ^. pvTitle
          , _uiTreatmentSku = v ^. pvSku
          , _uiTreatmentProductId = v ^. pvProductId
          , _uiTreatmentPrice = v ^. pvPrice
          , _uiTreatmentCurrency = v ^. pvCurrency
          , _uiTreatment = t ^. trTreatment
          }

  getCampaignStats domain cmpId = withConn $ \conn -> do
    mCmp <- runBeamPostgres conn
      $ runSelectReturningOne
      $ select
      $ do
        shop <- matchShop domain
        cmp <- all_ (db ^. campaigns)
        guard_ (_cmpShopId cmp `references_` shop)
        guard_ (_cmpId cmp ==. val_ cmpId)
        pure cmp

    case mCmp of
      Just cmp -> do
        tuples <- runBeamPostgres conn
          $ runSelectReturningList
          $ select
          $ do
            treatments <- filter_ ((==. val_ (_cmpId cmp)) . (^. trCmpId))
              $ all_ (db ^. treatments)

            variants <- filter_ ((==. (treatments ^. trProductVariantId)) . (^. pvId))
              $ all_ (db ^. productVariants)

            pure (treatments, variants)

        return $ groupResults tuples

        where
          groupResults tuples =
            CampaignStats
              { _cmpStatsCampaignId = _cmpId cmp
              , _cmpStatsCampaignName = _cmpName cmp
              , _cmpStatsStartDate = _cmpStart cmp
              , _cmpStatsEndDate = _cmpEnd cmp
              , _cmpStatsExperiments = map mkExpStats tuples & groupBySku & map snd
              }

          groupBySku :: [(Sku, ExperimentStats)] -> [(Sku, ExperimentStats)]
          groupBySku =  M.toList . M.fromListWith combineVariants
            where
              combineVariants e1 e2 = e1 & expStatsVariants %~ (<> (e2 ^. expStatsVariants))

          mkExpStats (treatment, variant) =
            (variant ^. pvSku,
            ExperimentStats
              { _expStatsSku = variant ^. pvSku
              , _expStatsUserCount = 0
              , _expStatsVariants =
                [
                  VariantStats
                  { _varStatsSvid = variant ^. pvVariantId
                  , _varStatsTreatment = treatment ^. trTreatment
                  , _varStatsUserCount = 0
                  , _varStatsPrice = variant ^. pvPrice
                  }
                ]
              }
              )


      Nothing -> do
        print "error ******"
        error "lol"
