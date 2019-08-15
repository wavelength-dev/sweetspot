{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SweetSpot.Database.Queries.Injectable where

import           Database.Beam
import           Database.Beam.Postgres

import           SweetSpot.Database.Schema

addUsers :: Connection -> IO ()
addUsers conn =
        runBeamPostgresDebug putStrLn conn
                $ runInsert
                $ insert (_users db)
                $ insertExpressions [User default_, User default_]

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
