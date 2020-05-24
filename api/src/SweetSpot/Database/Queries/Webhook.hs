module SweetSpot.Database.Queries.Webhook where

import Data.Aeson (Value)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full (deleteReturning, runPgDeleteReturningList)
import RIO
import SweetSpot.AppM (AppM (..))
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util (unsafeFindShopId, withConn)
import SweetSpot.Database.Schema

class Monad m => WebhookDB m where
  insertActionRequest :: ShopDomain -> ActionRequestType -> Value -> m ()
  uninstallShop :: ShopDomain -> m ()

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

  uninstallShop domain = withConn $ \conn -> do
    shopId <- unsafeFindShopId conn domain
    runBeamPostgres conn $ do
      users <- selectShopUsers shopId
      campaigns <- selectShopCampaigns shopId
      deleteSessions shopId
      deleteTreatments campaigns
      deleteProductVariants shopId
      deleteCheckoutItems shopId
      deleteCheckoutEvents shopId
      deleteUserExperiments campaigns
      deleteTreatments campaigns
      deleteUserCartTokens users
      deleteUsers users
      deleteCampaigns shopId
      deleteShop shopId

deleteProductVariants :: ShopId -> Pg [PVariantId]
deleteProductVariants shopId =
  runPgDeleteReturningList $
    deleteReturning
      (db ^. productVariants)
      (\v -> v ^. pvShopId ==. val_ shopId)
      (\v -> v ^. pvId)

deleteTreatments :: [CampaignId] -> Pg ()
deleteTreatments cmpIds = do
  runDelete $
    delete
      (db ^. treatments)
      (\t -> t ^. trCmpId `in_` map val_ cmpIds)

deleteCheckoutEvents :: ShopId -> Pg ()
deleteCheckoutEvents shopId =
  runDelete $
    delete
      (db ^. checkoutEvents)
      (\e -> e ^. cevShopId ==. val_ shopId)

deleteCheckoutItems :: ShopId -> Pg ()
deleteCheckoutItems shopId = do
  eventIds <- runSelectReturningList $ select $ do
    event <- all_ (db ^. checkoutEvents)
    item <- all_ (db ^. checkoutItems)
    guard_ (_ciCheckoutEventId item `references_` event)
    guard_ (event ^. cevShopId ==. val_ shopId)
    pure $ event ^. cevId
  runDelete $
    delete
      (db ^. checkoutItems)
      (\i -> i ^. ciCheckoutEventId `in_` map val_ eventIds)

deleteCampaigns :: ShopId -> Pg ()
deleteCampaigns shopId =
  runDelete $
    delete
      (db ^. campaigns)
      (\c -> c ^. cmpShopId ==. val_ shopId)

deleteUserCartTokens :: [UserId] -> Pg ()
deleteUserCartTokens userIds = do
  runDelete $
    delete
      (db ^. userCartTokens)
      (\t -> t ^. cartTokenUser `in_` map val_ userIds)

deleteUserExperiments :: [CampaignId] -> Pg ()
deleteUserExperiments cmpIds =
  runDelete $
    delete
      (db ^. userExperiments)
      (\e -> e ^. ueCmpId `in_` map val_ cmpIds)

deleteSessions :: ShopId -> Pg ()
deleteSessions shopId =
  runDelete $
    delete
      (db ^. sessions)
      (\s -> s ^. sessionShopId ==. val_ shopId)

deleteUsers :: [UserId] -> Pg ()
deleteUsers userIds =
  runDelete $
    delete (db ^. users) (\u -> u ^. usrId `in_` map val_ userIds)

deleteShop :: ShopId -> Pg ()
deleteShop shopId' =
  runDelete $
    delete (db ^. shops) (\s -> s ^. shopId ==. val_ shopId')

selectShopUsers :: ShopId -> Pg [UserId]
selectShopUsers shopId =
  runSelectReturningList $ select $ do
    campaign <- all_ (db ^. campaigns)
    experiment <- all_ (db ^. userExperiments)
    guard_ (_ueCmpId experiment `references_` campaign)
    guard_ (campaign ^. cmpShopId ==. val_ shopId)
    pure $ experiment ^. ueUserId

selectShopCampaigns :: ShopId -> Pg [CampaignId]
selectShopCampaigns shopId =
  runSelectReturningList $ select $ do
    campaign <- all_ (db ^. campaigns)
    guard_ (campaign ^. cmpShopId ==. val_ shopId)
    pure $ campaign ^. cmpId
