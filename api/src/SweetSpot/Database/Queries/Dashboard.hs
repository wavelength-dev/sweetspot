{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Queries.Dashboard where

import           Control.Lens
import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres

import           SweetSpot.Data.Common
import           SweetSpot.Database.Schema

createExperiment
        :: Connection
        -> (Sku, Svid, Svid, Price, Price, CampaignId, Text)
        -> IO ()
createExperiment conn (Sku s, Svid ctrl, Svid test, Price ctrlP, Price testP, CampaignId c, name)
        = do
                [exp] <-
                        runBeamPostgres conn
                        $ BeamExt.runInsertReturningList
                        $ insert (db ^. experiments)
                        $ insertExpressions
                                  [ Experiment { _expId          = default_
                                               , _expSku         = val_ s
                                               , _expProductName = val_ name
                                               }
                                  ]

                runBeamPostgres conn
                        $ runInsert
                        $ insert (db ^. campaignExperiments)
                        $ insertValues
                                  [ CampaignExperiment
                                            { _cmpForExp = CampaignKey c
                                            , _expForCmp = toExpKey exp
                                            }
                                  ]
                [cb, tb] <-
                        runBeamPostgres conn
                        $ BeamExt.runInsertReturningList
                        $ insert (db ^. buckets)
                        $ insertExpressions
                                  [ Bucket { _bktId       = default_
                                           , _bktType     = val_ "control"
                                           , _bktCtrlSvid = val_ ctrl
                                           , _bktTestSvid = val_ ctrl
                                           , _bktPrice    = val_ ctrlP
                                           }
                                  , Bucket { _bktId       = default_
                                           , _bktType     = val_ "test"
                                           , _bktCtrlSvid = val_ ctrl
                                           , _bktTestSvid = val_ test
                                           , _bktPrice    = val_ testP
                                           }
                                  ]

                runBeamPostgres conn
                        $ runInsert
                        $ insert (db ^. experimentBuckets)
                        $ insertValues
                                  [ ExperimentBucket { _expForBkt = toExpKey exp
                                                     , _bktForExp = toBktKey cb
                                                     }
                                  , ExperimentBucket { _expForBkt = toExpKey exp
                                                     , _bktForExp = toBktKey tb
                                                     }
                                  ]
    where
        toExpKey exp = exp ^. expId & ExperimentKey
        toBktKey bkt = bkt ^. bktId & BucketKey

-- | ---------------------------------------------------------------------------
-- | Stats
-- | ---------------------------------------------------------------------------
getCampaign :: Connection -> Text -> IO (Maybe Campaign)
getCampaign conn cmpId =
        runBeamPostgres conn
                $ runSelectReturningOne
                $ select
                $ filter_ (\c -> _cmpId c ==. val_ cmpId)
                $ all_ (_campaigns db)


getCampaignExperiments :: Connection -> CampaignId -> IO [Experiment]
getCampaignExperiments conn (CampaignId cmpId) =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                exps    <- all_ (db ^. experiments)
                cmpExps <- all_ (db ^. campaignExperiments)

                guard_ (_expForCmp cmpExps `references_` exps)
                guard_ (_cmpForExp cmpExps ==. cmpKey)

                pure exps
        where cmpKey = val_ (CampaignKey cmpId)

getExperimentBuckets :: Connection -> ExpId -> IO [Bucket]
getExperimentBuckets conn (ExpId eid) =
        runBeamPostgres conn $ runSelectReturningList $ select $ do
                expBkts <- all_ (db ^. experimentBuckets)
                bkts    <- all_ (db ^. buckets)
                guard_ (_bktForExp expBkts `references_` bkts)
                guard_ (_expForBkt expBkts ==. expKey)
                pure bkts
        where expKey = val_ (ExperimentKey (SqlSerial eid))
