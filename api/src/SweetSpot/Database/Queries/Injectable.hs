{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Queries.Injectable where

import           Database.Beam
import           Database.Beam.Postgres

import           SweetSpot.Database.Schema

-- addBuckets = runInsert $ insert (_buckets sweetspot) $ insertValues
--         [ Bucket 123 "control" 12345 12346 10.90
--         , Bucket 124 "test"    22345 22346 15.90
--         ]

-- addUsers :: Connection -> IO ()
-- addUsers conn =
--         runBeamPostgresDebug putStrLn conn
--                 $ runInsert
--                 $ insert (_users sweetspot)
--                 $ insertExpressions [User default_, User default_]

getAllBuckets :: Connection -> IO [Bucket]
getAllBuckets conn =
        runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select
                allBuckets
        where allBuckets = all_ (_buckets db)


getAllUsers :: Connection -> IO [User]
getAllUsers conn =
        runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select
                allUsers
        where allUsers = all_ (_users db)
