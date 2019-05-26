module SweetSpot.Main where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Global (readFloat)
import SweetSpot.AppM (AppM, ClientErr(..), runAppM)
import SweetSpot.Capability (ensureCampaign, ensureDeps, getUserBucket, getUserId, setUserId)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Constant (hiddenPriceId, idClassPattern)
import SweetSpot.Event (trackView)
import SweetSpot.Request (postLogPayload)
import Web.DOM.Document (getElementsByClassName, getElementsByTagName)
import Web.DOM.Element (getAttribute, setAttribute)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
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

collectCheckoutOptions :: Array String -> Effect (Array Element)
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
            Just id -> A.elem id variantIds

swapCheckoutVariantId :: Array UserBucket -> Array Element -> Effect Unit
swapCheckoutVariantId userBuckets els =
  traverse_ (\el -> getSuppleVariantId el >>= (\variantId ->
              case variantId of
                 Nothing -> pure unit
                 Just vId -> (setAttribute "value" (toString vId) el))
             ) els
  where
    getSuppleVariantId :: Element -> Effect (Maybe Number)
    getSuppleVariantId el = do
       attrValue <- getAttribute "value" el
       pure $ case attrValue of
         Nothing -> Nothing
         Just id -> case (A.find (\(UserBucket x) -> x._ubSvid == (readFloat id)) userBuckets) of
                      Nothing -> Nothing
                      Just (UserBucket ub) -> Just ub._ubSvid


-- Consider using DOM classList.remove method
removeClass :: String -> Element -> Effect Unit
removeClass className el = do
  current <- E.className el
  E.setClassName (S.replace pattern replacement current) el
  where
    pattern = S.Pattern className
    replacement = S.Replacement ""

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
applyExperiment (UserBucket { _ubSku, _ubPrice }) = do
  els <- liftEffect collectPriceEls
  liftEffect $ traverse_ maybeInjectPrice els
  liftEffect $ traverse_ (removeClass hiddenPriceId) els

  where
    maybeInjectPrice :: Element -> Effect Unit
    maybeInjectPrice el = do
      sku <- getIdFromPriceElement el
      match <- pure $ ((==) _ubSku) <$> sku
      case match of
        Just true -> log $ "Found match for " <> _ubSku <> " price would be " <> (toString _ubPrice)
        _ -> pure unit

app :: AppM Unit
app = do
  liftAff getDOMReady
  ensureDeps
  ensureCampaign
  uid <- getUserId
  bucket <- getUserBucket uid
  setUserId bucket
  applyExperiment bucket
  trackView bucket
  --log "Successfully applied experiments."

main :: Effect Unit
main = launchAff_ $ do
  res <- runAppM app
  liftEffect $ case res of
    Right _ -> pure unit
    Left (ClientErr { message }) -> do
      unhidePrice
      --C.error $ "Failed to apply experiments: " <> message
      launchAff_ $ forkAff $ postLogPayload message
