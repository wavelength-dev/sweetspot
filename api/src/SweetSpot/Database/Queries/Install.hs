module SweetSpot.Database.Queries.Install where

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions as BeamExt
import Database.Beam.Postgres
import RIO
import SweetSpot.AppM (AppM)
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util
  ( withConn,
  )
import SweetSpot.Database.Schema
import SweetSpot.Shopify.Types

class Monad m => InstallDB m where
  generateInstallNonce :: ShopDomain -> m Nonce
  getInstallNonce :: ShopDomain -> m (Maybe Nonce)
  deleteInstallNonce :: ShopDomain -> m ()
  createShop :: ShopDomain -> ShopInfo -> Text -> m ()
  getOAuthToken :: ShopDomain -> m (Maybe Text)

instance InstallDB AppM where
  generateInstallNonce shopDomain = withConn $ \conn -> do
    [row] <-
      runBeamPostgres conn
        $ BeamExt.runInsertReturningList
        $ insert (db ^. installNonces)
        $ insertExpressions
          [ InstallNonce
              { _installNonce = nonce_,
                _installShopDomain =
                  val_ shopDomain
              }
          ]
    pure $ row ^. installNonce

  getInstallNonce shopDomain = withConn $ \conn ->
    runBeamPostgres conn $ runSelectReturningOne $ select $ do
      rows <-
        filter_
          ((==. val_ shopDomain) . (^. installShopDomain))
          (all_ (db ^. installNonces))
      pure $ rows ^. installNonce

  deleteInstallNonce shopDomain = withConn $ \conn ->
    runBeamPostgres conn $ runDelete $
      delete
        (db ^. installNonces)
        ((==. val_ shopDomain) . (^. installShopDomain))

  createShop shopDomain shopInfo token = withConn $ \conn ->
    runBeamPostgres conn
      $ runInsert
      $ insert (db ^. shops)
      $ insertExpressions
        [ Shop
            { _shopId = shopId_,
              _shopCreated = nowUTC_,
              _shopDomain = val_ shopDomain,
              _shopOAuthToken = val_ token,
              _shopCountryCode = val_ $ shopInfo ^. shopInfoCountryCode,
              _shopCurrency = val_ $ shopInfo ^. shopInfoCurrency,
              _shopEmail = val_ $ shopInfo ^. shopInfoEmail,
              _shopMoneyFormat = val_ $ shopInfo ^. shopInfoMoneyFormat
            }
        ]

  getOAuthToken domain = withConn $ \conn ->
    runBeamPostgres conn $ runSelectReturningOne $ select $ do
      rows <-
        filter_
          ((==. val_ domain) . (^. shopDomain))
          (all_ (db ^. shops))
      pure $ rows ^. shopOAuthToken
