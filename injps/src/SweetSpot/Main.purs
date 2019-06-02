module SweetSpot.Main where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Global (readFloat)
import SweetSpot.AppM (AppM, ClientErr(..), runAppM)
import SweetSpot.Capability (ensureCampaign, ensureDeps, getUserBucket, getUserId, setUserId)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Constant (hiddenPriceId, idClassPattern)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document (getElementsByClassName, getElementsByTagName)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.HTMLElement (classList, fromElement)
import Web.HTML.Window (document)

getDOMReady :: Aff Unit
getDOMReady =
  makeAff \cb -> do
    listener <- eventListener (\_ -> cb (Right unit))
    doc <- window >>= document
    addEventListener domcontentloaded listener false (toEventTarget doc)
    pure nonCanceler

collectPriceEls :: Effect (Array Element)
collectPriceEls = do
  doc <- window >>= document
  els <- getElementsByClassName hiddenPriceId (toDocument doc)
  toArray els

collectCheckoutOptions :: Array Number -> Effect (Array Element)
collectCheckoutOptions variantIds = do
  doc <- window >>= document
  els <- getElementsByTagName "option" (toDocument doc) >>= toArray
  -- return any element with a value attribute value equal to one of variantIds
  A.filterA getIsKnownVariantOption els
  where
    getIsKnownVariantOption :: Element -> Effect Boolean
    getIsKnownVariantOption el = do
       optionId <- E.getAttribute "value" el
       pure $ case optionId of
            Nothing -> false
            Just id -> A.elem (readFloat id) variantIds

swapCheckoutVariantId :: Array UserBucket -> Array Element -> Effect Unit
swapCheckoutVariantId userBuckets els =
  traverse_ swapCheckoutIds els
  where
    swapCheckoutIds el = getSuppleVariantId el >>= (\variantId ->
      case variantId of
        Nothing -> pure unit
        -- The mutation needed for the live version below. Currently it only adds a class.
        -- Just vId -> (setAttribute "value" (toString vId) el))
        Just vId -> addClass ("sweetspot-swap-" <> (toString vId)) el)

    getMatchingUserBucket :: String -> Maybe UserBucket
    getMatchingUserBucket id = A.find (\(UserBucket userBucket) -> userBucket._ubOriginalSvid == (readFloat id)) userBuckets

    getUbTestSvid (UserBucket ub) = ub._ubTestSvid

    getSuppleVariantId :: Element -> Effect (Maybe Number)
    getSuppleVariantId el = do
       attrValue <- E.getAttribute "value" el
       pure $ attrValue >>= getMatchingUserBucket # map getUbTestSvid

removeClass :: String -> Element -> Effect Unit
removeClass className el =
  maybe (pure unit) (classList >=> remove' className) (fromElement el)
  where
    remove' = flip DTL.remove

addClass :: String -> Element -> Effect Unit
addClass className el = do
  current <- E.className el
  E.setClassName (current <> " " <> className) el

getIdFromPriceElement :: Element -> Effect (Maybe String)
getIdFromPriceElement el = do
  classNames <- (S.split $ S.Pattern " ") <$> E.className el
  let
    match = A.find (S.contains idClassPattern) classNames
    sku = A.last =<< (S.split $ S.Pattern "--") <$> match
  pure sku

unhidePrice :: Effect Unit
unhidePrice = do
  els <- collectPriceEls
  traverse_ (removeClass hiddenPriceId) els

-- We assume all elements with a hidden price also have a class identifying which SKU the price belongs to.
applyExperiment :: UserBucket -> AppM Unit
applyExperiment ub@(UserBucket { _ubSku, _ubPrice, _ubOriginalSvid }) = do
  els <- liftEffect collectPriceEls
  liftEffect $ traverse_ maybeInjectPrice els
  liftEffect $ traverse_ (removeClass hiddenPriceId) els
  -- This line is wrong, the ubSvid is the new price variant id, we need to know the id of the original
  checkoutOptions <- liftEffect $ collectCheckoutOptions [_ubOriginalSvid]
  liftEffect $ swapCheckoutVariantId [ub] checkoutOptions
  where
    matchStr = "sweetspot-match-" <> (toString _ubPrice)
    maybeInjectPrice :: Element -> Effect Unit
    maybeInjectPrice el = do
      mSku <- getIdFromPriceElement el
      match <- pure $ ((==) _ubSku) <$> mSku
      case match of
        Just true -> addClass matchStr el
        _ -> pure unit

app :: AppM Unit
app = do
  liftAff getDOMReady
  ensureDeps
  uid <- getUserId
  campaignId <- ensureCampaign uid
  bucket <- getUserBucket uid campaignId
  setUserId bucket
  applyExperiment bucket
  trackView bucket

main :: Effect Unit
main = launchAff_ $ do
  result <- runAppM app
  liftEffect $ case result of
    Right _ -> pure unit
    Left (ClientErr { message }) -> do
      unhidePrice
      launchAff_ $ forkAff $ postLogPayload message
