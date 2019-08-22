module SweetSpot.AppM where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Number.Format (toString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as C
import SweetSpot.Compatibility (hasFetch, hasPromise)
import SweetSpot.DOM (collectCheckoutOptions, collectPriceEls, getIdFromPriceElement, getPathname, removeClass, replacePathname, setPrice)
import SweetSpot.Data.Api (UserBucket)
import SweetSpot.Data.Constant (DryRunMode(..), campaignIdQueryParam, dryRunMode, hiddenPriceId, uidStorageKey, variantUrlPattern)
import SweetSpot.Data.Domain (CampaignId(..), UserId(..))
import SweetSpot.Data.Product (Sku(..))
import SweetSpot.Intl (formatNumber, numberFormat)
import SweetSpot.Longvadon (collectLongvadonCheckoutOptions, swapLongvadonCheckoutVariantId)
import SweetSpot.Request (UserBucketProvisions(..), fetchUserBuckets, postLogPayload)
import Web.DOM (Element)
import Web.DOM.Element as E
import Web.DOM.MutationObserver (mutationObserver, observe)
import Web.DOM.MutationRecord (target)
import Web.HTML (window)
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.Location (search)
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage (getItem, setItem)

data ShortCircuit = ReportErr { message :: String, payload :: String } | Noop

newtype AppM a = AppM (ExceptT ShortCircuit Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadThrowAppM :: MonadThrow ShortCircuit AppM

runAppM :: forall a. AppM a -> Aff (Either ShortCircuit a)
runAppM (AppM m) = runExceptT m

parseCampaignId :: String -> Maybe CampaignId
parseCampaignId queryString =
  let
    clean = fromMaybe queryString (S.stripPrefix (S.Pattern "?") queryString)
    kvPairs = S.split (S.Pattern "&") >>> map (S.split $ S.Pattern "=") $ clean
    campaignPred = \arr -> maybe false ((==) campaignIdQueryParam) (A.index arr 0)
    match = A.find campaignPred kvPairs
  in
   match >>= flip A.index 1 <#> CampaignId

mutateProductSource :: (NonEmptyArray UserBucket) -> AppM Unit
mutateProductSource buckets = liftEffect $ do
  elements <- collectLongvadonCheckoutOptions (map _._ubOriginalSvid buckets)
  swapLongvadonCheckoutVariantId buckets elements

applyPriceVariation :: NonEmptyArray UserBucket -> Element -> Effect Unit
applyPriceVariation userBuckets el = do
  mSku <- getIdFromPriceElement el
  let mBucket = mSku >>= (\(Sku sku) -> A.find ((==) sku <<< _._ubSku) userBuckets)
  maybe (pure unit) (\bucket -> maybeInjectPrice bucket._ubSku bucket._ubPrice) mBucket
  checkoutOptions <- liftEffect $ collectCheckoutOptions (map _._ubOriginalSvid userBuckets)
  liftEffect $ swapLongvadonCheckoutVariantId userBuckets checkoutOptions
  where
    maybeInjectPrice :: String -> Number -> Effect Unit
    maybeInjectPrice variantSku variantPrice = do
      mSku <- getIdFromPriceElement el
      let match = maybe false (\(Sku sku) -> sku == variantSku) mSku
      case match, dryRunMode of
        true, DryRun -> do
          nf <- numberFormat
          formattedPrice <- formatNumber variantPrice nf
          E.setAttribute "data-ssdr__price" formattedPrice el
        true, Live -> setPrice variantPrice el
        _, _ -> pure unit

ensureDeps :: AppM Unit
ensureDeps =
  case promise, fetch of
    true, true -> pure unit
    _, _ ->
      throwError (ReportErr { message: "Missing required dependencies"
                            , payload: "Promise: " <> (show promise) <> " Fetch: " <> (show fetch)
                            })
  where
    promise = hasPromise
    fetch = hasFetch

getUserId :: AppM (Maybe UserId)
getUserId = do
  mUid <- liftEffect $ window >>= localStorage >>= getItem uidStorageKey
  pure $ UserId <$> mUid

setUserId :: UserBucket -> AppM Unit
setUserId b =
  liftEffect $ window >>= localStorage >>= setItem uidStorageKey (toString b._ubUserId)

getUserBuckets :: UserBucketProvisions -> AppM (NonEmptyArray UserBucket)
getUserBuckets userBucketProvisions = do
  mBuckets <- liftAff $ fetchUserBuckets userBucketProvisions
  case fromArray <$> mBuckets of
       Right Nothing -> throwError (ReportErr { message: noBucketErr, payload: "" })
       Right (Just buckets) -> pure buckets
       Left err -> throwError (ReportErr { message: "Error fetching user buckets", payload: err })
  where
  noBucketErr = "User " <> (maybe "UnknownUid" unwrap mUserId) <> " has no buckets!"
  mUserId = case userBucketProvisions of
                 (UserAndCampaignId uid cid) -> Just uid
                 (OnlyUserId uid) -> Just uid
                 _ -> Nothing

log :: String -> AppM Unit
log msg = do
  -- Fork aff since we don't care about result
  _ <- liftAff $ forkAff $ postLogPayload msg
  liftEffect $ C.log msg

getCampaignId :: AppM (Maybe CampaignId)
getCampaignId = liftEffect $ window >>= location >>= search >>= pure <<< parseCampaignId

getUserBucketProvisions :: Maybe UserId -> Maybe CampaignId -> AppM UserBucketProvisions
getUserBucketProvisions Nothing Nothing = throwError Noop
getUserBucketProvisions (Just uid) (Just cid) = pure $ UserAndCampaignId uid cid
getUserBucketProvisions (Just uid) Nothing = pure $ OnlyUserId uid
getUserBucketProvisions Nothing (Just cid) = pure $ OnlyCampaignId cid

applyPriceVariations :: (NonEmptyArray UserBucket) -> AppM Unit
applyPriceVariations userBuckets = do
  priceElements <- liftEffect collectPriceEls
  let priceHTMLElements = A.catMaybes $ map fromElement priceElements
  -- It's unlikely but possible not all collected Elements are HTMLElements
  -- We should only add sweetspot ids to elements which are HTMLElements
  -- In case we made a mistake, we log a warning and continue with those elements which are HTMLElements
  _ <-
    liftEffect
      $ if A.length priceElements /= A.length priceHTMLElements then
          launchAff_ $ postLogPayload "WARN: some collected price elements are not HTMLElements"
        else
          pure unit
  liftEffect $ traverse_ (applyPriceVariation userBuckets) priceElements
  liftEffect $ traverse_ (removeClass hiddenPriceId) priceHTMLElements

attachPriceObserver :: (NonEmptyArray UserBucket) -> AppM Unit
attachPriceObserver buckets = do
  priceElements <- liftEffect collectPriceEls
  let cb = (\mrs _ ->
            case A.head mrs of
                Nothing -> C.log "No mutation records"
                Just mr -> target mr >>= \node ->
                  maybe
                    (C.log  "Node was not of type element")
                    (applyPriceVariation buckets)
                    (E.fromNode node))
  muOb <- liftEffect $ mutationObserver cb
  liftEffect $ for_ priceElements \el -> observe (E.toNode el) { childList: true } muOb

applyFacadeUrl :: AppM Unit
applyFacadeUrl = liftEffect $ do
  path <- getPathname
  when (isMatch path) (replacePathname $ strip path)
  where
    strip path = fromMaybe path $ S.stripSuffix (S.Pattern variantUrlPattern) path
    isMatch =
      S.split (S.Pattern "/")
        >>> A.last
        >>> maybe false (S.contains (S.Pattern variantUrlPattern))

unhidePrice :: Effect Unit
unhidePrice = liftEffect $ do
  priceElements <- collectPriceEls
  -- It's unlikely but possible not all collected Elements are HTMLElements
  -- We should only add sweetspot ids to elements which are HTMLElements
  -- In case we made a mistake, we log a warning and continue with those elements which are HTMLElements
  let
    priceHTMLElements = map fromElement priceElements

    anyInvalidElements = A.any isNothing priceHTMLElements
  _ <- when anyInvalidElements (launchAff_ $ postLogPayload "WARN: some collected price elements are not HTMLElements")
  traverse_ (removeClass hiddenPriceId) (A.catMaybes $ priceHTMLElements)

