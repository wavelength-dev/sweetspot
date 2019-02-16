module Supple.Component.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.AppM (Env)
import Supple.Component.Util (css)
import Supple.Data.Api (Experiments, Experiment)

type State =
  { experiments :: Experiments }

data Query a
  = UpdateExperiments Experiments a

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => H.Component HH.HTML Query Experiments Void m
component =
  H.component
    { initialState: \exps -> { experiments: exps }
    , render
    , eval
    , receiver: HE.input UpdateExperiments
    }
  where

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      UpdateExperiments newExps a -> do
        { experiments } <- H.get
        when (newExps /= experiments) $ H.modify_ _ { experiments = newExps }
        pure a

    renderExperiment :: forall i p . Experiment -> HH.HTML i p
    renderExperiment e =
      HH.a
        [HP.href $ "/#/experiment/" <> show e.exp_id]
        [HH.text e.name]

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [css "Polaris-Page"]
        [HH.div
        [css "Polaris-Page__Title"]
            [HH.h1
              [css "Polaris-DisplayText Polaris-DisplayText--sizeLarge"]
              [HH.text "Home"]],

         HH.div_ $ renderExperiment <$> state.experiments]
