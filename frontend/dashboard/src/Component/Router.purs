module Component.Router where

import Prelude

import AppM (Env)
import Capability.Experiment (class ManageExperiments)
import Capability.Navigate (class Navigate)
import Component.Experiment as Experiment
import Component.Home as Home
import Control.Monad.Reader (class MonadAsk)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State = { route :: Route }

data Query a
  = Navigate Route a

type ChildQuery = Coproduct2 Home.Query Experiment.Query

type ChildSlot = Either2 Unit Unit

component
  :: forall m
     . MonadAff m
    => MonadAsk Env m
    => Navigate m
    => ManageExperiments m
    => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState = { route: Home }

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (Navigate dest a ) = do
      { route } <- H.get
      when (route /= dest) do
        H.modify_ _ { route = dest }
      pure a

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { route } = case route of
      Home ->
        HH.slot' CP.cp1 unit Home.component unit absurd
      Experiment _ ->
        HH.slot' CP.cp2 unit Experiment.component unit absurd
