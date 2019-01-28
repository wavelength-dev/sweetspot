module Component.Router where

import AppM
import Prelude

import Capability.Navigate (class Navigate)
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { route :: Route }

data Query a
  = Navigate Route a

component
  :: forall m
     . MonadAff m
    => MonadAsk Env m
    => Navigate m
    => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState = { route: Home }

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Navigate dest a ) = do
      { route } <- H.get
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure a

    render :: State -> H.ComponentHTML Query
    render { route } =
      HH.div_ [
        case route of
          Home -> HH.h1_ [ HH.text "Home"]
          Experiment _ -> HH.h1_ [ HH.text "Experiment"],

        HH.button
        [ HE.onClick (HE.input_ $ Navigate (Experiment "Lolexp"))]
        [ HH.text "Navigate"]
      ]
