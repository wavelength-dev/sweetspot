module Injectable where

import Prelude

import Data.Array (uncons)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..)) as Method
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Console (log)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.XHR.EventTypes (error, load) as EventType
import Web.XHR.ResponseType (string) as ResponseType
import Web.XHR.XMLHttpRequest (open, response, send, toEventTarget, xmlHttpRequest)

type ApiExperiment
  = {bucket_price :: Number, bucket_sku :: String, bucket_svid :: Number, user_id :: Number}

type Experiment
  = {price :: Number, sku :: String, svid :: Number}

apiURL :: String
apiURL = "http://71a81ddd.ngrok.io/api/bucket"

getExperiments :: Aff (Maybe String)
getExperiments = do
  xhr <- xmlHttpRequest ResponseType.string
  successListener <- eventListener ?success
  failureListener <- eventListener ?failure
  addEventListener EventType.load successListener false (toEventTarget xhr)
  addEventListener EventType.error failureListener false (toEventTarget xhr)
  open (Left Method.GET) apiURL xhr
  send xhr

main :: Effect Unit
main = do
  expsRes <- getExperiments
  case expsRes of
    (Just expsJSON) -> log expsJSON
    Nothing -> log "No response!"
