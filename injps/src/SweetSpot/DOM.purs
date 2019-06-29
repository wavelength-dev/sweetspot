module SweetSpot.DOM where

import Prelude
import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Number.Format (toString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Global (readFloat)
import SweetSpot.Data.Api (UserBucket(..))
import SweetSpot.Data.Constant (hiddenPriceId, idClassPattern)
import SweetSpot.Intl (formatNumber, numberFormat)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Document (getElementsByClassName, getElementsByTagName)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (setTextContent)
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

collectCheckoutOptions :: NonEmptyArray Number -> Effect (Array Element)
collectCheckoutOptions variantIds = do
  doc <- window >>= document
  els <- getElementsByTagName "option" (toDocument doc) >>= toArray
  -- return any element with a value attribute value equal to one of variantIds
  A.filterA getIsKnownVariantOption els
  where
  getIsKnownVariantOption :: Element -> Effect Boolean
  getIsKnownVariantOption el = do
    optionId <- E.getAttribute "value" el
    pure
      $ case optionId of
          Nothing -> false
          Just id -> A.elem (readFloat id) variantIds

swapCheckoutVariantId :: NonEmptyArray UserBucket -> Array Element -> Effect Unit
swapCheckoutVariantId userBuckets els = traverse_ swapCheckoutIds els
  where
  swapCheckoutIds el =
    getOptionVariantId el
      >>= ( \variantId -> case variantId of
          Nothing -> pure unit
          -- The mutation needed for the live version below. Currently it only adds a class.
          -- Just vId -> (setAttribute "value" (toString vId) el))
          Just vId -> addClass ("sweetspot-swap-" <> (toString vId)) el
        )

  getMatchingUserBucket :: String -> Maybe UserBucket
  getMatchingUserBucket id = A.find (\(UserBucket userBucket) -> userBucket._ubOriginalSvid == (readFloat id)) userBuckets

  getOptionVariantId :: Element -> Effect (Maybe Number)
  getOptionVariantId el = do
    attrValue <- E.getAttribute "value" el
    pure $ attrValue >>= getMatchingUserBucket # map (\(UserBucket ub) -> ub._ubTestSvid)

removeClass :: String -> Element -> Effect Unit
removeClass className el = maybe (pure unit) (classList >=> remove' className) (fromElement el)
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
unhidePrice = collectPriceEls >>= traverse_ (removeClass hiddenPriceId)

setPrice :: Number -> Element -> Effect Unit
setPrice price el = do
  nf <- numberFormat
  formattedPrice <- formatNumber price nf
  setTextContent formattedPrice (E.toNode el)
