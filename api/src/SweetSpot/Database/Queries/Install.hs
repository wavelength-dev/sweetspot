module SweetSpot.Database.Queries.Install where

import           Control.Lens
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
                                               as BeamExt
import           Database.Beam.Postgres
import           Data.Text                      ( Text )
import           SweetSpot.AppM                 ( AppM )
import           SweetSpot.Data.Common
import           SweetSpot.Database.Schema
import           SweetSpot.Database.Queries.Util
                                                ( withConn )

class Monad m => InstallDB m where
  generateInstallNonce :: ShopDomain -> m Nonce
  getInstallNonce :: ShopDomain -> m (Maybe Nonce)
  deleteInstallNonce :: ShopDomain -> m ()
  createShop :: ShopDomain -> Text -> m ()

instance InstallDB AppM where
        generateInstallNonce shopDomain = withConn $ \conn -> do
                [row] <-
                        runBeamPostgres conn
                        $ BeamExt.runInsertReturningList
                        $ insert (db ^. installNonces)
                        $ insertExpressions
                                  [ InstallNonce
                                            { _installNonce      = nonce_
                                            , _installShopDomain =
                                                    val_ shopDomain
                                            }
                                  ]
                pure $ row ^. installNonce

        getInstallNonce shopDomain = withConn $ \conn ->
                runBeamPostgres conn $ runSelectReturningOne $ select $ do
                        rows <- filter_
                                ((==. val_ shopDomain) . (^. installShopDomain))
                                (all_ (db ^. installNonces))
                        pure $ rows ^. installNonce

        deleteInstallNonce shopDomain = withConn $ \conn ->
                runBeamPostgres conn $ runDelete $ delete
                        (db ^. installNonces)
                        ((==. val_ shopDomain) . (^. installShopDomain))

        createShop shopDomain token = withConn $ \conn ->
                runBeamPostgres conn
                        $ runInsert
                        $ insert (db ^. shops)
                        $ insertExpressions
                                  [ Shop { _shopId         = shopId_
                                         , _shopCreated    = now_
                                         , _shopDomain     = val_ shopDomain
                                         , _shopOauthToken = val_ token
                                         }
                                  ]
