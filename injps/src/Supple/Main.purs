module Supple.Main where

import Prelude
import Supple.AppM (AppM, runAppM)
import Supple.Data.Api (UserBucket(..))

import Data.Array as A
import Data.Either (Either(Right))
import Data.Foldable (null, traverse_)
import Data.Maybe (Maybe)
import Data.Number (fromString)
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Console (warn)
import Supple.Capability (getUserBuckets, getUserId)
import Web.DOM.Document (getElementsByClassName)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.Window (document)
import Web.Storage.Storage as St

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

unhidePrice :: Effect Unit
unhidePrice = do
  els <- collectPriceEls
  if (null els)
    then warn $ "expected to unhide prices but no elements with class " <> hiddenPriceId <>" found."
    else traverse_ (removeClass hiddenPriceId) els

getIdFromPriceElement :: Element -> Effect (Maybe Number)
getIdFromPriceElement el = do
  classNames <- (S.split $ S.Pattern " ") <$> E.className el
  let
    match = A.find (S.contains $ S.Pattern "supple__price_id--") classNames
    pid = fromString =<< A.last =<< (S.split $ S.Pattern "--") <$> match
  pure pid

setUserId :: St.Storage -> Number -> Effect Unit
setUserId st uid = St.setItem uidStorageKey (show uid) st

f :: UserBucket -> String
f (UserBucket ub) = ub._ubSku

app :: AppM Unit
app = do
  liftAff getDOMReady
  uid <- getUserId
  bs <- getUserBuckets uid
  a <- pure $ f bs
  pure unit

main :: Effect Unit
main = launchAff_ $ runAppM { logLevel: "info" } app
