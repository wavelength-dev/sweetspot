{-# LANGUAGE FlexibleContexts #-}

module SweetSpot.Database.Queries.Util where

import           Control.Lens                   ( (^.) )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader.Class     ( asks )
import           Database.Beam
import           Database.Beam.Postgres
import           Data.Pool                      ( Pool, withResource )
import           SweetSpot.AppM                 ( AppM
                                                , AppCtx(..)
                                                )
import           SweetSpot.Database.Schema

withConn :: (Connection -> IO a) -> AppM a
withConn f = do
        pool <- asks _getDbPool
        liftIO . withResource pool $ \conn -> f conn


withConnIO :: Pool Connection -> (Connection -> IO a) -> IO a
withConnIO pool f = liftIO . withResource pool $ \conn -> f conn

matchShop domain = filter_ ((==. val_ domain) . (^. shopDomain)) (all_ (db ^. shops))
