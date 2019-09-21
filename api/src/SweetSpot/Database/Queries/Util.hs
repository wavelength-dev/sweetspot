module SweetSpot.Database.Queries.Util where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader.Class     ( asks )
import           Database.Beam.Postgres
import           Data.Pool                      ( withResource )
import           SweetSpot.AppM                 ( AppM
                                                , AppCtx(..)
                                                )

withConn :: (Connection -> IO a) -> AppM a
withConn f = do
        pool <- asks _getDbPool
        liftIO . withResource pool $ \conn -> f conn
