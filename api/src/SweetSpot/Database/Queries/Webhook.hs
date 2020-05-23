module SweetSpot.Database.Queries.Webhook where

import Data.Aeson (Value)
import Database.Beam
import Database.Beam.Postgres
import RIO
import SweetSpot.AppM (AppM (..))
import SweetSpot.Data.Common (ActionRequestType, ShopDomain)
import SweetSpot.Database.Queries.Util (unsafeFindShopId, withConn)
import SweetSpot.Database.Schema

class Monad m => WebhookDB m where
  insertActionRequest :: ShopDomain -> ActionRequestType -> Value -> m ()

instance WebhookDB AppM where
  insertActionRequest domain requestType payload = withConn $ \conn -> do
    -- This query is called through webhook which means there will
    -- be an install
    shopId <- unsafeFindShopId conn domain
    runBeamPostgres conn
      $ runInsert
      $ insert (db ^. actionRequests)
      $ insertExpressions
        [ ActionRequest
            { _actionRequestId = pgGenUUID_,
              _actionRequestShopId = val_ (ShopKey shopId),
              _actionRequestType = val_ requestType,
              _actionRequestPayload = val_ (PgJSONB payload)
            }
        ]
