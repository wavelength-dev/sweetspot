module Injectable where

import Prelude

import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..)) as Method
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), launchAff_, makeAff, nonCanceler, parallel, sequential)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.HTMLDocument (toEventTarget) as HDoc
import Web.HTML.Window (document)
import Web.XHR.EventTypes (load) as EventType
import Web.XHR.ResponseType (string) as ResponseType
import Web.XHR.XMLHttpRequest (open, response, send, xmlHttpRequest, abort)
import Web.XHR.XMLHttpRequest (toEventTarget) as XHR

type URL = String

type ApiExperiment
  = {bucket_price :: Number, bucket_sku :: String, bucket_svid :: Number, user_id :: Number}

type Experiment
  = {price :: Number, sku :: String, svid :: Number}

apiURL :: String
apiURL = "http://localhost/api/bucket?uid=1"

getExperiments :: Aff String
getExperiments =
  makeAff \cb ->
    liftEffect $ do
      xhr <- xmlHttpRequest ResponseType.string
      successListener <- eventListener
                         \_ -> do
                            res <- response xhr
                            cb $ note (error "Fail") res

      addEventListener EventType.load successListener false (XHR.toEventTarget xhr)
      open (Left Method.GET) apiURL xhr
      send xhr
      pure $ Canceler \_ -> liftEffect $ abort xhr

getDOMReady :: Aff Unit
getDOMReady = 
  makeAff \cb ->
    liftEffect $ do
      listener <- eventListener (\_ -> cb (Right unit))
      doc <- (window >>= document)
      addEventListener domcontentloaded listener false (HDoc.toEventTarget doc)
      -- pure $ Canceler \_ -> liftEffect $ removeEventListener domcontentloaded listener false (HDoc.toEventTarget doc)
      pure nonCanceler

whenReady :: Aff (Tuple Unit String)
whenReady = sequential $
  Tuple <$> parallel (getDOMReady)
        <*> parallel (getExperiments)

main :: Effect Unit
main = launchAff_ $ do
  Tuple a b <- whenReady
  liftEffect $ log b
