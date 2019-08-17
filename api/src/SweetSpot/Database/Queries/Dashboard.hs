{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Database.Queries.Dashboard where

import           Data.Text                      ( Text )
import           Database.Beam
import           Database.Beam.Postgres

import           SweetSpot.Database.Schema


getCampaign :: Connection -> Text -> IO [Campaign]
getCampaign conn cmpId =
        runBeamPostgresDebug putStrLn conn
                $ runSelectReturningList
                $ select
                $ filter_ (\c -> _cmpId c ==. val_ cmpId)
                $ all_ (_campaigns db)


getCampaignExperiments :: Connection -> Text -> IO [Experiment]
getCampaignExperiments conn cmpId =
        runBeamPostgresDebug putStrLn conn
                $ runSelectReturningList
                $ select
                $ do
                          experiments         <- all_ (_experiments db)
                          campaignExperiments <- all_ (_campaignExperiments db)

                          guard_
                                  (             _expForCmp campaignExperiments
                                  `references_` experiments
                                  )

                          pure experiments
