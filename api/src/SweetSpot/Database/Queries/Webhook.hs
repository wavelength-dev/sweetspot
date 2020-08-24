module SweetSpot.Database.Queries.Webhook where

import Data.Aeson (Value)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full (deleteReturning, runPgDeleteReturningList)
import RIO
import SweetSpot.AppM (AppM (..))
import SweetSpot.Data.Common
import SweetSpot.Database.Queries.Util
  ( findShopId,
    unsafeFindShopId,
    withConn,
  )
import SweetSpot.Database.Schema

class Monad m => WebhookDB m where
  insertActionRequest :: ShopDomain -> ActionRequestType -> Value -> m ()
  uninstallShop :: ShopDomain -> m ()

instance WebhookDB AppM where
  insertActionRequest domain requestType payload = withConn $ \conn ->
    -- This query is called through webhook which means there will
    -- be an install
    runBeamPostgres conn $ do
      shopId <- unsafeFindShopId domain
      runInsert
        $ insert (db ^. actionRequests)
        $ insertExpressions
          [ ActionRequest
              { _actionRequestId = pgGenUUID_,
                _actionRequestShopId = val_ (ShopKey shopId),
                _actionRequestType = val_ requestType,
                _actionRequestPayload = val_ (PgJSONB payload)
              }
          ]

  uninstallShop domain = withConn $ \conn ->
    runBeamPostgres conn $ do
      mShopId <- findShopId domain
      case mShopId of
        Nothing -> pure ()
        Just shopId -> do
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
          deleteAppCharge shopId
          deleteShop shopId

deleteProductVariants :: ShopId -> Pg [PVariantId]
deleteProductVariants shopId =
  runPgDeleteReturningList $
    deleteReturning
      (db ^. productVariants)
      (\v -> v ^. productVariantShopId ==. val_ shopId)
      (\v -> v ^. productVariantId)

deleteTreatments :: [CampaignId] -> Pg ()
deleteTreatments cmpIds = do
  runDelete $
    delete
      (db ^. treatments)
      (\t -> t ^. treatmentCampaignId `in_` map val_ cmpIds)

deleteCheckoutEvents :: ShopId -> Pg ()
deleteCheckoutEvents shopId =
  runDelete $
    delete
      (db ^. checkoutEvents)
      (\e -> e ^. checkoutEventShopId ==. val_ shopId)

deleteCheckoutItems :: ShopId -> Pg ()
deleteCheckoutItems shopId = do
  eventIds <- runSelectReturningList $ select $ do
    event <- all_ (db ^. checkoutEvents)
    item <- all_ (db ^. checkoutItems)
    guard_ (_checkoutItemCheckoutEventId item `references_` event)
    guard_ (event ^. checkoutEventShopId ==. val_ shopId)
    pure $ event ^. checkoutEventId
  runDelete $
    delete
      (db ^. checkoutItems)
      (\i -> i ^. checkoutItemCheckoutEventId `in_` map val_ eventIds)

deleteCampaigns :: ShopId -> Pg ()
deleteCampaigns shopId =
  runDelete $
    delete
      (db ^. campaigns)
      (\c -> c ^. campaignShopId ==. val_ shopId)

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
      (\e -> e ^. userExperimentCampaignId `in_` map val_ cmpIds)

deleteSessions :: ShopId -> Pg ()
deleteSessions shopId =
  runDelete $
    delete
      (db ^. sessions)
      (\s -> s ^. sessionShopId ==. val_ shopId)

deleteUsers :: [UserId] -> Pg ()
deleteUsers userIds =
  runDelete $
    delete (db ^. users) (\u -> u ^. userId `in_` map val_ userIds)

deleteAppCharge :: ShopId -> Pg ()
deleteAppCharge shopId' =
  runDelete $
    delete
      (db ^. appCharges)
      (\c -> c ^. appChargeShopId ==. val_ shopId')

deleteShop :: ShopId -> Pg ()
deleteShop shopId' =
  runDelete $
    delete (db ^. shops) (\s -> s ^. shopId ==. val_ shopId')

selectShopUsers :: ShopId -> Pg [UserId]
selectShopUsers shopId =
  runSelectReturningList $ select $ do
    campaign <- all_ (db ^. campaigns)
    experiment <- all_ (db ^. userExperiments)
    guard_ (_userExperimentCampaignId experiment `references_` campaign)
    guard_ (campaign ^. campaignShopId ==. val_ shopId)
    pure $ experiment ^. userExperimentUserId

selectShopCampaigns :: ShopId -> Pg [CampaignId]
selectShopCampaigns shopId =
  runSelectReturningList $ select $ do
    campaign <- all_ (db ^. campaigns)
    guard_ (campaign ^. campaignShopId ==. val_ shopId)
    pure $ campaign ^. campaignId
