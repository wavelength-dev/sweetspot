module SweetSpot.Database.Queries.Util where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Pool (Pool, withResource)
import Database.Beam
import Database.Beam.Postgres
import RIO
import RIO.Partial (fromJust)
import SweetSpot.AppM
import SweetSpot.Data.Common
import SweetSpot.Database.Schema

withConn :: (Connection -> IO a) -> AppM a
withConn f = do
  pool <- asks (^. ctxDbPool)
  liftIO . withResource pool $ \conn -> f conn

withConnIO :: Pool Connection -> (Connection -> IO a) -> IO a
withConnIO pool f = liftIO . withResource pool $ \conn -> f conn

matchShop domain = filter_ ((==. val_ domain) . (^. shopDomain)) (all_ (db ^. shops))

selectShopMoneyFormat domain =
  (^. shopMoneyFormat)
    <$> filter_ ((==. val_ domain) . (^. shopDomain)) (all_ (db ^. shops))

unsafeFindShopId :: Connection -> ShopDomain -> IO ShopId
unsafeFindShopId conn domain =
  fromJust
    <$> ( runBeamPostgres conn
            $ runSelectReturningOne
            $ select
            $ view shopId <$> matchShop domain
        )
