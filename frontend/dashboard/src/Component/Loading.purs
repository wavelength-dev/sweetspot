module Supple.Component.Loading where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)

data Query a = Noop a
type State = Unit

component
  :: forall m
   . MonadAff m
  => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const unit
    , render
    , receiver: const Nothing
    , eval
    }
  where

    render :: State -> H.ComponentHTML Query
    render _ = HH.text "Loading..."

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Noop a) = pure a
