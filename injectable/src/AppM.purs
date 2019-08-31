module SweetSpot.AppM where

import Prelude

import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT, throwError)
import Data.Array (find) as Array
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as C
import SweetSpot.Api (TestMapProvisions(..), fetchTestMaps, postLogPayload)
import SweetSpot.Compatibility (hasFetch, hasPromise)
import SweetSpot.DOM (getIdFromPriceElement, getPathname, removeClass, replacePathname, setPrice)
import SweetSpot.Data.Api (TestMap)
import SweetSpot.Data.Config (DryRunMode(..), campaignIdQueryParam, dryRunMode, hiddenPriceId, idClass, uidStorageKey, variantUrlPattern)
import SweetSpot.Data.Domain (CampaignId(..), UserId(..))
import SweetSpot.Intl (formatNumber, numberFormat)
import SweetSpot.LibertyPrice as LP
import SweetSpot.Longvadon as Lv
import SweetSpot.SiteCapabilities as SiteC
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.MutationObserver (mutationObserver, observe)
import Web.DOM.MutationRecord (target)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.HTMLElement (fromElement)
import Web.HTML.Location (hostname, search)
import Web.HTML.Window (localStorage, location)
import Web.HTML.Window (location) as Win
import Web.Storage.Storage (getItem, setItem)

data ShortCircuit = ReportErr { message :: String, payload :: String } | Noop String

newtype AppM a = AppM (ExceptT ShortCircuit Aff a)

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
priceElementSelector = QuerySelector $ "[class*=" <> idClass <> "]"

getSiteId :: Effect Site
getSiteId = do
  siteHostname <- window >>= Win.location >>= hostname
  pure $ case siteHostname of
    "longvadon.com" -> Longvadon
    "libertyprice.myshopify.com" -> LibertyPrice
    _ -> Unknown siteHostname

parseCampaignId :: String -> Maybe CampaignId
parseCampaignId queryString =
  let
    clean = fromMaybe queryString (S.stripPrefix (S.Pattern "?") queryString)
    kvPairs = S.split (S.Pattern "&") >>> map (S.split $ S.Pattern "=") $ clean
    campaignPred = \arr -> maybe false ((==) campaignIdQueryParam) (A.index arr 0)
    match = A.find campaignPred kvPairs
  in
   match >>= flip A.index 1 <#> CampaignId

setCheckout :: Site -> Array TestMap -> AppM Unit
setCheckout siteId testMaps = do
  case siteId of
    Longvadon -> liftEffect $ Lv.setCheckout testMaps
    LibertyPrice -> liftEffect $ LP.setCheckout testMaps
    _ -> throwError $ ReportErr { message : "Site not recognized, can't control checkout, hostname was " <> (show siteId), payload : "" }

applyPriceVariation :: NonEmptyArray TestMap -> Element -> Effect Unit
applyPriceVariation testMaps el = do
  mElementSku <- getIdFromPriceElement el
  let mTestMap = mElementSku >>= (\sku -> Array.find (_.sku >>> (==) sku) testMaps)
  case mTestMap, dryRunMode of
    (Just testMap), DryRun -> do
          nf <- numberFormat
          formattedPrice <- formatNumber (Int.toNumber testMap.swapPrice) nf
          Element.setAttribute "data-ssdr__price" formattedPrice el
    (Just testMap), Live -> setPrice (Int.toNumber testMap.swapPrice) el
    Nothing, _ -> pure unit

ensureDeps :: AppM Unit
ensureDeps =
  case promise, fetch of
    true, true -> pure unit
    _, _ -> throwError (ReportErr { message: "Missing required dependencies"
                                  , payload: "Promise: " <> (show promise) <> " Fetch: " <> (show fetch)
                                  })
  where
    promise = hasPromise
    fetch = hasFetch

getUserId :: Effect (Maybe UserId)
getUserId = do
  mUid <- liftEffect $ window >>= localStorage >>= getItem uidStorageKey
  pure $ UserId <$> mUid

setUserId :: TestMap -> Effect Unit
setUserId testMap =
  liftEffect $ window >>= localStorage >>= setItem uidStorageKey testMap.userId

getUserBuckets :: TestMapProvisions -> AppM (NonEmptyArray TestMap)
getUserBuckets userBucketProvisions = do
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

log :: String -> AppM Unit
log msg = do
  -- Fork aff since we don't care about result
  _ <- liftAff $ forkAff $ postLogPayload msg
  liftEffect $ C.log msg

getCampaignId :: Effect (Maybe CampaignId)
getCampaignId = window >>= location >>= search >>= pure <<< parseCampaignId

getUserBucketProvisions :: Maybe UserId -> Maybe CampaignId -> AppM TestMapProvisions
getUserBucketProvisions Nothing Nothing = throwError $ Noop "No userId or campaign url parameter. Exiting..."
getUserBucketProvisions (Just uid) (Just cid) = pure $ UserAndCampaignId uid cid
getUserBucketProvisions (Just uid) Nothing = pure $ OnlyUserId uid
getUserBucketProvisions Nothing (Just cid) = pure $ OnlyCampaignId cid

applyPriceVariations :: (NonEmptyArray TestMap) -> Effect Unit
applyPriceVariations userBuckets = do
  priceElements <- SiteC.queryDocument priceElementSelector
  let priceHTMLElements = A.catMaybes $ map fromElement priceElements
  -- It's unlikely but possible not all collected Elements are HTMLElements
  -- We should only add sweetspot ids to elements which are HTMLElements
  -- In case we made a mistake, we log a warning and continue with those elements which are HTMLElements
  _ <-
    if A.length priceElements /= A.length priceHTMLElements then
      launchAff_ $ postLogPayload "WARN: some collected price elements are not HTMLElements"
    else
      pure unit
  traverse_ (applyPriceVariation userBuckets) priceElements
  traverse_ (removeClass hiddenPriceId) priceHTMLElements

attachPriceObserver :: (NonEmptyArray TestMap) -> Effect Unit
attachPriceObserver testMaps = do
  priceElements <- SiteC.queryDocument priceElementSelector
  let cb = (\mrs _ ->
            case A.head mrs of
                Nothing -> C.log "No mutation records"
                Just mr -> target mr >>= \node ->
                  maybe
                    (C.log  "Node was not of type element")
                    (applyPriceVariation testMaps)
                    (Element.fromNode node))
  muOb <- mutationObserver cb
  for_ priceElements \el -> observe (Element.toNode el) { childList: true } muOb

applyFacadeUrl :: Effect Unit
applyFacadeUrl = do
  path <- getPathname
  when (isMatch path) (replacePathname $ strip path)
  where
    strip path = fromMaybe path $ S.stripSuffix (S.Pattern variantUrlPattern) path
    isMatch =
      S.split (S.Pattern "/")
        >>> A.last
        >>> maybe false (S.contains (S.Pattern variantUrlPattern))

unhidePrice :: Effect Unit
unhidePrice = do
  priceElements <- SiteC.queryDocument priceElementSelector
  -- It's unlikely but possible not all collected Elements are HTMLElements
  -- We should only add sweetspot ids to elements which are HTMLElements
  -- In case we made a mistake, we log a warning and continue with those elements which are HTMLElements
  let
    priceHTMLElements = map fromElement priceElements

    anyInvalidElements = A.any isNothing priceHTMLElements
  _ <- when anyInvalidElements (launchAff_ $ postLogPayload "WARN: some collected price elements are not HTMLElements")
  traverse_ (removeClass hiddenPriceId) (A.catMaybes $ priceHTMLElements)


fixCartItemUrls :: Site -> Effect Unit
fixCartItemUrls siteId =
  when (siteId == Longvadon) Lv.replaceTestVariantUrlOnCart
