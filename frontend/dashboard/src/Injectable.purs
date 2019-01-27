module Injectable where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (Method(..)) as Method
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.Event.EventTarget (addEventListener)
import Web.XHR.ResponseType (string)
import Web.XHR.XMLHttpRequest (open, response, send, toEventTarget, xmlHttpRequest)
-- import Web.DOM.Document (getElementsByClassName)
-- import Web.DOM.Element (toNode)
-- import Web.DOM.HTMLCollection (item)
-- import Web.DOM.Node (setTextContent)
-- import Web.HTML (window)
-- import Web.HTML.HTMLDocument (toDocument)
-- import Web.HTML.Window (document)

type ApiExperiment = {
  bucket_price :: Number,
  bucket_sku :: String,
  bucket_svid :: Number,
  user_id :: Number
}

type Experiment = {
  price :: Number,
  sku :: String,
  svid :: Number
}

data QueryParam = QueryParam String (Maybe String)

stringifyQueryString :: List QueryParam -> String
stringifyQueryString Nil = ""
stringifyQueryString (QueryParam key (Just value) : Nil) = key <> "=" <> value
stringifyQueryString (QueryParam key Nothing : xs) = "" <> stringifyQueryString xs
stringifyQueryString (QueryParam key (Just value) : xs) = key
  <> "="
  <> value
  <> "&"
  <> stringifyQueryString xs

apiURL :: String
apiURL = "http://57630551.ngrok.io/api/bucket"

getExperiments :: Effect (Maybe String)
getExperiments = do
  xhr <- xmlHttpRequest string
  _ <- open (Left Method.GET) apiURL xhr
  _ <- send xhr
  target <- toEventTarget xhr
  makeAff

main :: Effect Unit
main = do
  expsRes <- getExperiments
  case expsRes of
       (Just expsJSON) -> log expsJSON
       Nothing -> log "No response!"
  -- window' <- window
  -- doc <- document window'
  -- el <-  getElementsByClassName "hello" $ toDocument doc
  -- maybeEl <- item 0 el
  -- case maybeEl of
  --   Just e -> setTextContent "Hello world!" $ toNode e
  --   Nothing -> pure unit
