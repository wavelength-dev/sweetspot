module SweetSpot.AppM where

import Prelude
import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import SweetSpot.Api (TestMapProvisions(..), fetchTestMaps)
import SweetSpot.Compatibility (hasFetch, hasPromise)
import SweetSpot.Data.Config as Config
import SweetSpot.Data.Domain (CampaignId(..), Sku, TestMap, UserId(..), VariantId(..))
import SweetSpot.LibertyPrice as LP
import SweetSpot.Longvadon as Lv
import SweetSpot.SiteCapabilities as SiteC
import Web.HTML (window)
import Web.HTML.HTMLElement (fromElement) as HTMLElement
import Web.HTML.Location (hostname)
import Web.HTML.Window (localStorage)
import Web.HTML.Window as Win
import Web.Storage.Storage (getItem, setItem)

data ShortCircuit
  = ReportErr { message :: String, payload :: String }
  | Noop

newtype AppM a
  = AppM (ExceptT ShortCircuit Aff a)

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

data Site
  = Longvadon
  | LibertyPrice

derive instance eqSite :: Eq Site

instance showSite :: Show Site where
  show Longvadon = "longvadon.com"
  show LibertyPrice = "libertyprice.myshopify.com"

getSiteId :: Effect (Either String Site)
getSiteId =
  window
    >>= Win.location
    >>= hostname
    >>= \siteHostname ->
        pure
          $ case siteHostname of
              "longvadon.com" -> Right Longvadon
              "libertyprice.myshopify.com" -> Right LibertyPrice
              _ -> Left siteHostname

ensureDeps :: AppM Unit
ensureDeps = case promise, fetch of
  true, true -> pure unit
  _, _ ->
    throwError
      ( ReportErr
          { message: "Missing required dependencies"
          , payload: "Promise: " <> (show promise) <> " Fetch: " <> (show fetch)
          }
      )
  where
  promise = hasPromise

  fetch = hasFetch

getUserId :: Effect (Maybe UserId)
getUserId = do
  mUid <- liftEffect $ window >>= localStorage >>= getItem Config.uidStorageKey
  pure $ UserId <$> mUid

setUserId :: TestMap -> Effect Unit
setUserId testMap = liftEffect $ window >>= localStorage >>= setItem Config.uidStorageKey testMap.userId

getTestMaps :: TestMapProvisions -> AppM (NonEmptyArray TestMap)
getTestMaps userBucketProvisions = do
  mBuckets <- liftAff $ fetchTestMaps userBucketProvisions
  case fromArray <$> mBuckets of
    Left err -> throwError (ReportErr { message: err, payload: "" })
    Right Nothing -> throwError (ReportErr { message: noBucketErr, payload: "" })
    Right (Just testMaps) -> pure testMaps
  where
  noBucketErr = "User " <> userId <> " has no testMaps!"

  userId = case userBucketProvisions of
    (UserAndCampaignId uid cid) -> unwrap uid
    (OnlyUserId uid) -> unwrap uid
    _ -> "Unknown UserId"

readCampaignId :: Effect (Maybe CampaignId)
readCampaignId = SiteC.getUrlParam "sscid" >>= map CampaignId >>> pure

getUserBucketProvisions :: Maybe UserId -> Maybe CampaignId -> AppM TestMapProvisions
getUserBucketProvisions mUserId mCampaignId = case mUserId, mCampaignId of
  Nothing, Nothing -> throwError $ Noop
  (Just uid), (Just cid) -> pure $ UserAndCampaignId uid cid
  (Just uid), Nothing -> pure $ OnlyUserId uid
  Nothing, (Just cid) -> pure $ OnlyCampaignId cid

attachSiteObservers :: Site -> Map VariantId TestMap -> Effect Unit
attachSiteObservers site testMapsMap = case site of
  Longvadon -> Lv.attachObservers testMapsMap
  LibertyPrice -> LP.observePrices testMapsMap

applyFacadeUrl :: Effect Unit
applyFacadeUrl = do
  path <- SiteC.getPathname
  when (isMatch path) (SiteC.replacePathname $ strip path)
  where
  strip path = fromMaybe path $ String.stripSuffix (String.Pattern Config.variantUrlPattern) path

  isMatch =
    String.split (String.Pattern "/")
      >>> Array.last
      >>> maybe false (String.contains (String.Pattern Config.variantUrlPattern))

fixCartItemUrls :: Site -> Effect Unit
fixCartItemUrls siteId = when (siteId == Longvadon) Lv.convertSsvCollectionUrls

-- | This function collects all elements tagged with a sweetspot id, and sets them to their controlled price. Sadly, there's many ways in which LibertyPrice and Longvadon set prices on user interaction, this only covers simple cases.
setControlledPrices :: Map VariantId TestMap -> Effect Unit
setControlledPrices testMapsMap = do
  priceElements <- SiteC.queryDocument SiteC.priceElementSelector
  let
    priceHTMLElements = Array.catMaybes $ map HTMLElement.fromElement priceElements
  traverse_ (SiteC.setControlledPrice testMapsMap) priceElements

-- It's unlikely but possible not all collected Elements are HTMLElements
-- We should only add sweetspot ids to elements which are HTMLElements
revealPrices :: Effect Unit
revealPrices = SiteC.queryDocument SiteC.priceElementSelector >>= traverse_ SiteC.revealPrice

-- TODO: too low level for AppM, find a better place
getTestMapsByTargetId :: NonEmptyArray TestMap -> Map VariantId TestMap
getTestMapsByTargetId = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple (VariantId testMap.targetId) testMap

-- TODO: too low level for AppM, find a better place
getTestMapsBySku :: NonEmptyArray TestMap -> Map Sku TestMap
getTestMapsBySku = map toKeyValuePair >>> Map.fromFoldable
  where
  toKeyValuePair testMap = Tuple testMap.sku testMap
