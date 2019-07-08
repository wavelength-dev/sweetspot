module SweetSpot.AppM where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Data.Array (head)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.String as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as C
import SweetSpot.Capability (class AppCapability)
import SweetSpot.Compatibility (hasFetch, hasPromise)
import SweetSpot.DOM (collectCheckoutOptions, collectPriceEls, getIdFromPriceElement, removeClass, setPrice, swapCheckoutVariantId)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Constant (hiddenPriceId, uidStorageKey)
import SweetSpot.Request (fetchUserBuckets, postLogPayload)
import Web.DOM (Element)
import Web.DOM.Element as E
import Web.DOM.MutationObserver (mutationObserver, observe)
import Web.DOM.MutationRecord (target)
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
    kvPairs = S.split (S.Pattern "&") >>> map (S.split $ S.Pattern "=") $ clean
    campaignPred = \arr -> maybe false ((==) "sscid") (A.index arr 0)
    match = A.find campaignPred kvPairs
  in
   match >>= flip A.index 1

applyPriceVariation :: NonEmptyArray UserBucket -> Element -> Effect Unit
applyPriceVariation userBuckets el = do
  mSku <- getIdFromPriceElement el
  let mBucket = mSku >>= (\sku -> A.find (\(UserBucket ub) -> ub._ubSku == sku) userBuckets)
  case mBucket of
       Nothing -> pure unit
       Just (UserBucket bucket) -> maybeInjectPrice bucket._ubSku bucket._ubPrice
  checkoutOptions <- liftEffect $ collectCheckoutOptions (map (\(UserBucket ub) -> ub._ubOriginalSvid) userBuckets)
  liftEffect $ swapCheckoutVariantId userBuckets checkoutOptions
  where
    maybeInjectPrice :: String -> Number -> Effect Unit
    maybeInjectPrice variantSku variantPrice = do
      mSku <- getIdFromPriceElement el
      match <- pure $ ((==) variantSku) <$> mSku
      case match of
        Just true -> setPrice variantPrice el
        _ -> pure unit

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

  getUserBuckets uid campaignId = do
    mBuckets <- liftAff $ fetchUserBuckets uid campaignId
    case fromArray <$> mBuckets of
      Right Nothing -> throwError (ClientErr { message: "User " <> (fromMaybe "UnknownUid" uid) <> " has no buckets!", payload: "" })
      Right (Just buckets) -> pure buckets
      Left err -> throwError (ClientErr { message: "Error fetching user buckets", payload: err })

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

  -- We assume all elements with a hidden price also have a class identifying which SKU the price belongs to.
  applyPriceVariations userBuckets = do
    els <- liftEffect collectPriceEls
    liftEffect $ traverse_ (applyPriceVariation userBuckets) els
    let res = traverse (removeClass hiddenPriceId) els
    pure $ case res of
      Nothing -> Nothing
      Just _ -> Just unit

  attachPriceObserver buckets = do
    priceElements <- liftEffect collectPriceEls
    let cb = (\mrs _ ->
             case head mrs of
                  Nothing -> C.log "No mutation records"
                  Just mr -> target mr >>= \node ->
                             case E.fromNode node of
                                  Nothing -> C.log "Node was not of type element"
                                  Just el -> applyPriceVariation buckets el
                                  )
    muOb <- liftEffect $ mutationObserver cb
    liftEffect $ for_ priceElements \el -> observe (E.toNode el) { childList: true } muOb
