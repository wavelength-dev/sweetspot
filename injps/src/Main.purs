module Main where

import Prelude

import Data.Array (index)
import Data.Either (Either(Left, Right), note)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(Canceler), launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Web.DOM.Document (getElementsByClassName)
import Web.DOM.Element (className)
import Web.DOM.HTMLCollection (toArray)
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

unhidePrice :: Effect Unit
unhidePrice = do
  doc <- (window >>= document)
  els <- getElementsByClassName "supple__price--hidden" (toDocument doc)
  elsArray <- toArray els
  classNames <- case (index elsArray 0) of
       Nothing -> pure Nothing
       Just el -> (className el) >>= (\c -> pure $ Just c)
  log $ fromMaybe "no el" classNames
  pure unit
  -- mempty $ map (removeAttribute "supple__price--hidden") elsArray

main :: Effect Unit
main = launchAff_ do
  _ <- getDOMReady
  -- exp <- getExperiments
  _ <- liftEffect unhidePrice
  liftEffect $ log "Done!"
