module Supple.Main where

import Prelude
import Data.Array (toUnfoldable)
import Data.Either (Either(Right))
import Data.Foldable (null, traverse_)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console (log, warn)
import Supple.Request (fetchUserBuckets)
import Web.DOM.Document (getElementsByClassName)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.Window (document, localStorage)
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

collectPriceEls :: Effect (List Element)
collectPriceEls = do
  doc <- (window >>= document)
  els <- getElementsByClassName hiddenPriceId (toDocument doc)
  elArr <- toArray els
  pure $ toUnfoldable elArr

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

getUserId :: St.Storage -> Effect (Maybe String)
getUserId = St.getItem "supple_uid"

main :: Effect Unit
main = launchAff_ do
  _ <- getDOMReady
  uid <- liftEffect $ do
    w <-  window
    storage <- localStorage w
    getUserId storage
  bs <- fetchUserBuckets uid
  liftEffect $ case bs of
    Just _ -> log "got something"
    Nothing -> log "got nothing"
  _ <- liftEffect unhidePrice
  liftEffect $ log "Done!"
