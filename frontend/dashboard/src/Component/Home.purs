module Component.Home where

import Prelude

import AppM (Env)
import Capability.Experiment (class ManageExperiments, getExperiments)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader (class MonadAsk)
import Data.Api (Experiment)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State =
  { experiments :: Maybe (Array Experiment) }

data Query a
  = Initialize a
  | LoadExperiments a

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => Navigate m
  => ManageExperiments m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState: const { experiments: Nothing }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      Initialize a -> do
        exps <- getExperiments
        H.modify_ _ { experiments = exps }
        pure a
      LoadExperiments a -> do
        pure a

    renderExperiment :: forall i p. Experiment -> H.HTML i p
    renderExperiment e =
      HH.text e.sku

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div_
        [
          HH.h1_ [ HH.text "Home"],

          case state.experiments of
            Just es -> maybe (HH.text "") (HH.text <<< _.name) (A.head es)
            Nothing -> HH.text ""

        ]
