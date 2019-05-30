module SweetSpot.AppM where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as C
import SweetSpot.Capability (class AppCapability)
import SweetSpot.Compatibility (hasFetch, hasPromise)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Constant (uidStorageKey)
import SweetSpot.Request (fetchUserBuckets, postLogPayload)
import Web.HTML (window)
import Web.HTML.Location (search)
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage (getItem, setItem)

newtype ClientErr = ClientErr
  { message :: String
  , payload :: String
  }

newtype AppM a = AppM (ExceptT ClientErr Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadThrowAppM :: MonadThrow ClientErr AppM

runAppM :: forall a. AppM a -> Aff (Either ClientErr a)
runAppM (AppM m) = runExceptT m

parseCampaignId :: String -> Maybe String
parseCampaignId qs =
  let
    clean = S.drop 1 qs
    pairs = S.split (S.Pattern "&") clean
    campaignPred = S.contains (S.Pattern "campaign=")
    match = A.find campaignPred pairs
  in
   match >>= pure <<< (S.split $ S.Pattern "=") >>= flip A.index 1

instance appCapabilityAppM :: AppCapability AppM where
  ensureDeps =
    case Tuple promise fetch of
      Tuple true true -> pure unit
      Tuple _ _ ->
        throwError (ClientErr { message: "Missing required dependencies"
                              , payload: "Promise: " <> (show promise) <> " Fetch: " <> (show fetch)
                              })
   where
     promise = hasPromise
     fetch = hasFetch

  getUserId = liftEffect $ window >>= localStorage >>= getItem uidStorageKey

  setUserId (UserBucket b) =
    liftEffect $ window >>= localStorage >>= setItem uidStorageKey (toString b._ubUserId)

  getUserBucket uid campaignId = do
    bucket <- liftAff $ fetchUserBuckets uid campaignId
    case bucket of
      Right b -> pure b
      Left err -> throwError (ClientErr { message: "Error fetching user bucket", payload: err })

  log msg = do
    -- Fork aff since we don't care about result
    _ <- liftAff $ forkAff $ postLogPayload msg
    liftEffect $ C.log msg

  ensureCampaign mUid = do
    case mUid of
      Just _ -> pure Nothing
      Nothing -> do
        campaignId <- liftEffect $ window >>= location >>= search >>= pure <<< parseCampaignId
        case campaignId of
          Nothing -> throwError (ClientErr { message: "No url campaign parameter", payload: "" })
          Just id -> pure $ Just id
