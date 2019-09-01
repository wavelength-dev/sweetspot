module SweetSpot.AppM where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import SweetSpot.Api (TestMapProvisions(..), fetchTestMaps, postLogPayload)
import SweetSpot.Compatibility (hasFetch, hasPromise)
import SweetSpot.Data.Config (DryRunMode(..))
import SweetSpot.Data.Config as Config
import SweetSpot.Data.Domain (CampaignId(..), TestMap, UserId(..), TestMapsMap)
import SweetSpot.Intl (formatNumber, numberFormat)
import SweetSpot.LibertyPrice as LP
import SweetSpot.Longvadon as Lv
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.MutationObserver (MutationObserver, mutationObserver, observe)
import Web.DOM.MutationRecord (MutationRecord)
import Web.DOM.MutationRecord (target) as MutationRecord
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.Location (hostname, search)
import Web.HTML.Window (localStorage, location)
import Web.HTML.Window (location) as Win
import Web.Storage.Storage (getItem, setItem)

data ShortCircuit
  = ReportErr { message :: String, payload :: String }
  | Noop String

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

-- instance domActionAppM :: DomAction AppM where
--   getAttribute = SiteC.getAttribute
--   setAttribute = SiteC.setAttribute
--   queryDocument = SiteC.queryDocument
runAppM :: forall a. AppM a -> Aff (Either ShortCircuit a)
runAppM (AppM m) = runExceptT m

data Site
  = Longvadon
  | LibertyPrice
  | Unknown String

derive instance eqSite :: Eq Site

instance showSite :: Show Site where
  show Longvadon = "longvadon.com"
  show LibertyPrice = "libertyprice.myshopify.com"
  show (Unknown hostname) = hostname

priceElementSelector :: QuerySelector
priceElementSelector = QuerySelector $ "[class*=" <> Config.idClass <> "]"

getSiteId :: Effect Site
getSiteId = do
  siteHostname <- window >>= Win.location >>= hostname
  pure
    $ case siteHostname of
        "longvadon.com" -> Longvadon
        "libertyprice.myshopify.com" -> LibertyPrice
        _ -> Unknown siteHostname

parseCampaignId :: String -> Maybe CampaignId
parseCampaignId queryString =
  let
    clean = fromMaybe queryString (String.stripPrefix (String.Pattern "?") queryString)

    kvPairs = String.split (String.Pattern "&") >>> map (String.split $ String.Pattern "=") $ clean

    campaignPred = \arr -> maybe false ((==) Config.campaignIdQueryParam) (Array.index arr 0)

    match = Array.find campaignPred kvPairs
  in
    match >>= flip Array.index 1 <#> CampaignId

setCheckout :: Site -> TestMapsMap -> AppM Unit
setCheckout siteId testMaps = do
  case siteId of
    Longvadon -> liftEffect $ Lv.setCheckout testMaps
    LibertyPrice -> liftEffect $ LP.setCheckout testMaps
    _ -> throwError $ ReportErr { message: "Site not recognized, can't control checkout, hostname was " <> (show siteId), payload: "" }

applyPriceVariation :: TestMapsMap -> Element -> Effect Unit
applyPriceVariation testMaps el = do
  mElementSku <- SiteC.getIdFromPriceElement el
  let
    mTestMap = mElementSku >>= (\sku -> Array.find (_.sku >>> (==) sku) testMaps)
  case mTestMap, Config.dryRunMode of
    (Just testMap), DryRun -> do
      nf <- numberFormat
      formattedPrice <- formatNumber (Int.toNumber testMap.swapPrice) nf
      Element.setAttribute "data-ssdr__price" formattedPrice el
    (Just testMap), Live -> SiteC.setPrice (Int.toNumber testMap.swapPrice) el
    Nothing, _ -> pure unit

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
    Left err -> throwError (ReportErr { message: "Error fetching user testMaps", payload: err })
    Right Nothing -> throwError (ReportErr { message: noBucketErr, payload: "" })
    Right (Just testMaps) -> pure testMaps
  where
  noBucketErr = "User " <> (maybe "UnknownUid" unwrap mUserId) <> " has no testMaps!"

  mUserId = case userBucketProvisions of
    (UserAndCampaignId uid cid) -> Just uid
    (OnlyUserId uid) -> Just uid
    _ -> Nothing

getCampaignId :: Effect (Maybe CampaignId)
getCampaignId = window >>= location >>= search >>= pure <<< parseCampaignId

getUserBucketProvisions :: Maybe UserId -> Maybe CampaignId -> AppM TestMapProvisions
getUserBucketProvisions Nothing Nothing = throwError $ Noop "No userId or campaign url parameter. Exiting..."

getUserBucketProvisions (Just uid) (Just cid) = pure $ UserAndCampaignId uid cid

getUserBucketProvisions (Just uid) Nothing = pure $ OnlyUserId uid

getUserBucketProvisions Nothing (Just cid) = pure $ OnlyCampaignId cid

-- It's unlikely but possible not all collected Elements are HTMLElements
-- We should only add sweetspot ids to elements which are HTMLElements
applyPriceVariations :: TestMapsMap -> Effect Unit
applyPriceVariations userBuckets = do
  priceElements <- SiteC.queryDocument priceElementSelector
  let priceHTMLElements = Array.catMaybes $ map fromElement priceElements
  traverse_ (applyPriceVariation userBuckets) priceElements
  traverse_ (SiteC.removeClass Config.hiddenPriceId) priceHTMLElements

type MutationCallback = Array MutationRecord → MutationObserver → Effect Unit

attachObserver :: MutationCallback -> TestMapsMap -> Array Element -> Effect Unit
attachObserver callback testMaps elements = do
  muOb <- mutationObserver callback
  for_ elements \el -> observe (Element.toNode el) { childList: true } muOb

attachPriceObserver :: Site -> TestMapsMap -> Effect Unit
attachPriceObserver site testMaps = do
  elements <- SiteC.queryDocument priceElementSelector
  attachObserver callback testMaps elements
  where
  callback mutationRecords _ = case Array.head mutationRecords of
    Nothing -> launchAff_ $ postLogPayload "Mutation observer got called without mutation records"
    Just mr ->
      MutationRecord.target mr >>= \node ->
        case Element.fromNode node of
          Nothing -> launchAff_ $ postLogPayload "Observed node was not of type Element"
          Just element -> case site of
            Longvadon -> do
              applyPriceVariation testMaps element
              Lv.setCheckoutButton testMaps
            _ -> pure unit

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

-- It's unlikely but possible not all collected Elements are HTMLElements
-- We should only add sweetspot ids to elements which are HTMLElements
unhidePrice :: Effect Unit
unhidePrice =
  SiteC.queryDocument priceElementSelector
    >>= (map fromElement)
    >>> Array.catMaybes
    >>> pure
    >>= traverse_ (SiteC.removeClass Config.hiddenPriceId)

fixCartItemUrls :: Site -> Effect Unit
fixCartItemUrls siteId = when (siteId == Longvadon) Lv.convertSsvCollectionUrls
