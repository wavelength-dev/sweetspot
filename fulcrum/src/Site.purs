module Fulcrum.Site where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (oneOf) as Foldable
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import QueryString (QueryParam(..))
import QueryString (parseQueryString) as QueryString
import Web.DOM (Document)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as EventTarget
import Web.HTML (window) as HTML
import Web.HTML.Event.EventTypes (domcontentloaded) as EventTypes
import Web.HTML.HTMLDocument (readyState, toDocument) as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Location (search) as Location
import Web.HTML.Window as Window

getDocument :: Effect Document
getDocument = HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure

awaitDomReady :: Aff Unit
awaitDomReady =
  makeAff \callback -> do
    rs <- HTMLDocument.readyState =<< Window.document =<< HTML.window
    case rs of
      Loading -> do
        et <- Window.toEventTarget <$> HTML.window
        listener <- EventTarget.eventListener (\_ -> callback (Right unit))
        EventTarget.addEventListener EventTypes.domcontentloaded listener false et
        pure $ effectCanceler (EventTarget.removeEventListener EventTypes.domcontentloaded listener false et)
      _ -> do
        callback (Right unit)
        pure nonCanceler

getUrlParam :: String -> Effect (Maybe String)
getUrlParam targetKey = do
  queryString <- HTML.window >>= Window.location >>= Location.search
  queryString
    # QueryString.parseQueryString
    >>> map matchQueryParam
    >>> Foldable.oneOf
    >>> pure
  where
  matchQueryParam :: Either String QueryParam -> Maybe String
  matchQueryParam = case _ of
    Right (QueryParam key (Just value)) | key == targetKey -> Just value
    _ -> Nothing
