module Fulcrum.Site where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as EventTarget
import Web.HTML (window) as HTML
import Web.HTML.Event.EventTypes (domcontentloaded) as EventTypes
import Web.HTML.HTMLDocument (readyState) as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Window as Window

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
