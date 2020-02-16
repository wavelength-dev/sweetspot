{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
              t <- filter_ ((==. val_ (_cmpId cmp)) . (^. trCmpId))
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

  getCampaignStats domain cmpId' = withConn $ \conn -> do
    mCmp <- runBeamPostgres conn
      $ runSelectReturningOne
      $ select
      $ do
        shop <- matchShop domain
        cmp <- all_ (db ^. campaigns)
        guard_ (_cmpShopId cmp `references_` shop)
        guard_ (_cmpId cmp ==. val_ cmpId')
        pure cmp

    case mCmp of
      Just cmp -> do
        [ctrlConversion] <- runBeamPostgres conn
          $ runSelectReturningList
          $ select
          $ (,) <$> userRevenueArrForTreatment (cmp ^. cmpId) 0
                <*> nonConvertersForTreatment (cmp ^. cmpId) 0

        [testConversion] <- runBeamPostgres conn
          $ runSelectReturningList
          $ select
          $ (,) <$> userRevenueArrForTreatment (cmp ^. cmpId) 1
                <*> nonConvertersForTreatment (cmp ^. cmpId) 1

        rows <- runBeamPostgres conn
          $ runSelectReturningList
          $ select
          $ do

            let usPerVar = usersPerVariantInCampaign cmp

            variant <- all_ (db ^. productVariants)
            treatment <- all_ (db ^. treatments)
            campaign <- all_ (db ^. campaigns)

            userCount <- fromMaybe_ 0 . snd <$>
                leftJoin_ usPerVar (\(varId, count) -> varId ==. variant ^. pvId)

            guard_ (_trProductVariantId treatment `references_` variant)
            guard_ (_trCmpId treatment `references_` campaign)
            guard_ (campaign ^. cmpId ==. val_ (_cmpId cmp))

            pure (treatment, variant, userCount)

        return $ mkCampaignStats rows ctrlConversion testConversion

        where
          groupBySku :: [(Sku, ExperimentStats)] -> [(Sku, ExperimentStats)]
          groupBySku =  M.toList . M.fromListWith combineVariants
            where
              combineVariants e1 e2 = e1 & expStatsVariants %~ mappend (e2 ^. expStatsVariants)

          mkCampaignStats rows (ctrlRevs, nonConvCtrl) (testRevs, nonConvTest) =
            CampaignStats
              { _cmpStatsCampaignId = _cmpId cmp
              , _cmpStatsCampaignName = _cmpName cmp
              , _cmpStatsStartDate = _cmpStart cmp
              , _cmpStatsEndDate = _cmpEnd cmp
              , _cmpStatsExperiments = map mkExpStats rows & groupBySku & map snd
              , _cmpStatsConvertersControl = ctrlRevs
              , _cmpStatsNonConvertersControl = nonConvCtrl
              , _cmpStatsConvertersTest = testRevs
              , _cmpStatsNonConvertersTest = nonConvTest
              }

          mkExpStats (treatment, variant, userCount) =
            ( variant ^. pvSku,

              ExperimentStats
                { _expStatsSku = variant ^. pvSku
                , _expStatsUserCount = 0
                , _expStatsVariants =
                  [
                    VariantStats
                    { _varStatsSvid = variant ^. pvVariantId
                    , _varStatsTreatment = treatment ^. trTreatment
                    , _varStatsUserCount = userCount
                    , _varStatsPrice = variant ^. pvPrice
                    }
                  ]
                }
            )


      Nothing -> do
        print "error ******"
        error "lol"


usersPerVariantInCampaign cmp =
  aggregate_ (\(var, ue) -> (group_ (var ^. pvId), count_ (ue ^. ueUserId))) $ do
      userExp <- all_ (db ^. userExperiments)
      treatment <- all_ (db ^. treatments)
      variant <- all_ (db ^. productVariants)

      let cmpId = val_ (_cmpId cmp)

      guard_ (userExp ^. ueCmpId ==. cmpId)
      guard_ (userExp ^. ueTreatment ==. treatment ^. trTreatment)
      guard_ (treatment ^. trCmpId ==. cmpId)
      guard_ (_trProductVariantId treatment `references_` variant)

      pure (variant, userExp)


userRevenueArrForTreatment cmpId' treatment' =
  aggregate_ (\(_, rev) -> pgArrayAgg (fromMaybe_ 0 rev)) $ do
    aggregate_ (\(userId, revenue) -> (group_ userId, sum_ revenue)) $ do
        user <- all_ (db ^. users)
        event <- all_ (db ^. checkoutEvents)
        item <- all_ (db ^. checkoutItems)
        variant <- all_ (db ^. productVariants)
        treatment <- all_ (db ^. treatments)

        guard_ (_cevUserId event `references_` user)
        guard_ (event ^. cevCmpId ==. val_ cmpId')
        guard_ (_ciCheckoutEventId item `references_` event)
        guard_ (item ^. ciSvid ==. variant ^. pvVariantId)
        guard_ (_trProductVariantId treatment `references_` variant)
        guard_ (treatment ^. trTreatment ==. treatment')

        let
          quantity = cast_ (item ^. ciQuantity) double
          price = cast_ (variant ^. pvPrice) double
          revenue = quantity * price

        pure (user ^. usrId, revenue)

nonConvertersForTreatment cmpId' treatment' = do
  userEvents <- aggregate_ (\(userId, eventId) -> (group_ userId, as_ @Int (count_ eventId))) $ do
      user <- all_ (db ^. users)
      experiment <- all_ (db ^. userExperiments)

      guard_ (_ueUserId experiment `references_` user)
      guard_ (experiment ^. ueTreatment ==. val_ treatment')
      guard_ (experiment ^. ueCmpId ==. val_ cmpId')

      event <- leftJoin_ (all_ (db ^. checkoutEvents))
        (\ce -> _cevUserId ce `references_` user)

      pure (user ^. usrId, event ^. cevId)

  guard_ (snd userEvents ==. 0)

  aggregate_ (const countAll_) (pure userEvents)
