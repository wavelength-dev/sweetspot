module Main where

import Prelude

import Data.Array (toUnfoldable)
import Data.Either (Either(Left, Right), isRight, note)
import Data.Foldable (null, traverse_)
import Data.HTTP.Method (Method(GET))
import Data.List (List)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), attempt, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console (log, warn)
import Effect.Exception (error)
import Milkis as M
import Milkis.Impl.Window (windowFetch)
import Test.Spec.Assertions (shouldEqual)
import Web.DOM.Document (getElementsByClassName)
import Web.DOM.Element (removeAttribute)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Internal.Types (Element)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toDocument, toEventTarget)
import Web.HTML.Window (document)
import Web.XHR.EventTypes (load) as EventType
import Web.XHR.ResponseType (string) as ResponseType
import Web.XHR.XMLHttpRequest (open, response, send, xmlHttpRequest, abort)
import Web.XHR.XMLHttpRequest (toEventTarget) as XHR

type URL = String

type ApiExperiment
  = { bucket_price :: Number
    , bucket_sku :: String
    , bucket_svid :: Number
    , user_id :: Number
    }

type Experiment
  = { price :: Number
    , sku :: String
    , svid :: Number
    }

apiURL :: String
apiURL = "http://localhost/api/bucket?uid=1"

getExperiments :: Aff String
getExperiments =
  makeAff \cb -> do
    xhr <- xmlHttpRequest ResponseType.string
    successListener <- eventListener \_ -> do
                          res <- response xhr
                          cb $ note (error "Fail") res
    addEventListener EventType.load successListener false (XHR.toEventTarget xhr)
    open (Left GET) apiURL xhr
    send xhr
    pure $ Canceler \_ -> liftEffect $ abort xhr

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

unhidePrice :: Effect Unit
unhidePrice = do
  els <- collectPriceEls
  if (null els)
    then warn $ "expected to unhide prices but no elements with class " <> hiddenPriceId <>" found."
    else traverse_ (removeAttribute hiddenPriceId) els

fetch :: M.Fetch
fetch = M.fetch windowFetch

trackView :: Aff Unit
trackView = do
  let
    opts =
      { method: M.getMethod
      , body: "{}"
      , headers: M.makeHeaders { "Content-Type": "application/json" }
      }
  result <- attempt $ fetch (M.URL "localhost:8082/health") opts
  isRight result `shouldEqual` true

main :: Effect Unit
main = launchAff_ do
  _ <- getDOMReady
  -- exp <- getExperiments
  _ <- liftEffect unhidePrice
  _ <- trackView
  liftEffect $ log "Done!"
