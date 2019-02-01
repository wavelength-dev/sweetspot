module Injectable where

import Prelude

import Data.Array (uncons)
import Data.Either (Either(..), note)
import Data.HTTP.Method (Method(..)) as Method
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), launchAff_, makeAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.XHR.EventTypes (error, load) as EventType
import Web.XHR.ResponseType (string) as ResponseType
import Web.XHR.XMLHttpRequest (open, response, send, toEventTarget, xmlHttpRequest, abort)

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

      addEventListener EventType.load successListener false (toEventTarget xhr)
      open (Left Method.GET) apiURL xhr
      send xhr
      pure $ Canceler \_ -> liftEffect $ abort xhr

main :: Effect Unit
main = launchAff_ $ do
  expsRes <- getExperiments
  liftEffect $ log expsRes
