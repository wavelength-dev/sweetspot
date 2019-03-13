module Supple.Main where

import Prelude

import Data.Array as A
import Data.Either (Either(Right))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Supple.AppM (AppM, runAppM)
import Supple.Capability (getUserBucket, getUserId)
import Supple.Data.Api (UserBucket(..))
import Web.DOM.Document (getElementsByClassName)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node as N
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.Window (document)


getDOMReady :: Aff Unit
getDOMReady =
  makeAff \cb -> do
    listener <- eventListener (\_ -> cb (Right unit))
    doc <- (window >>= document)
    addEventListener domcontentloaded listener false (toEventTarget doc)
    pure nonCanceler

hiddenPriceId :: String
hiddenPriceId = "supple__price--hidden"

uidStorageKey :: String
uidStorageKey = "supple_uid"

collectPriceEls :: Effect (Array Element)
collectPriceEls = do
  doc <- window >>= document
  els <- getElementsByClassName hiddenPriceId (toDocument doc)
  toArray els

removeClass :: String -> Element -> Effect Unit
removeClass className el = do
  current <- E.className el
  E.setClassName (S.replace pattern replacement current) el
  where
    pattern = S.Pattern className
    replacement = S.Replacement ""

getIdFromPriceElement :: Element -> Effect (Maybe String)
getIdFromPriceElement el = do
  classNames <- (S.split $ S.Pattern " ") <$> E.className el
  let
    match = A.find (S.contains $ S.Pattern "supple__price_id--") classNames
    sku = A.last =<< (S.split $ S.Pattern "--") <$> match
  pure sku


applyExperiment :: UserBucket -> AppM Unit
applyExperiment (UserBucket { _ubSku, _ubPrice }) = do
  els <- liftEffect collectPriceEls
  liftEffect $ traverse_ maybeInjectPrice els
  liftEffect $ traverse_ (removeClass hiddenPriceId) els

  where
    textPrice = show _ubPrice

    maybeInjectPrice :: Element -> Effect Unit
    maybeInjectPrice el = do
      sku <- getIdFromPriceElement el
      match <- pure $ ((==) _ubSku) <$> sku
      case match of
        Just true -> N.setTextContent textPrice (E.toNode el)
        _ ->  pure unit

app :: AppM Unit
app = do
  liftAff getDOMReady
  uid <- getUserId
  bucket <- getUserBucket uid
  applyExperiment bucket
  pure unit

main :: Effect Unit
main = launchAff_ $ runAppM app
