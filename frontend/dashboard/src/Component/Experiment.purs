module Supple.Component.Experiment where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Supple.AppM (Env)
import Supple.Data.Api (Experiment)

type State = {experiment :: Experiment}

data Query a = UpdateExperiment Experiment a

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => H.Component HH.HTML Query Experiment Void m
component =
  H.component
    { initialState: \exp -> { experiment: exp }
    , render
    , eval
    , receiver: HE.input UpdateExperiment
    }
  where

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      UpdateExperiment newExp a -> do
        { experiment } <- H.get
        when (newExp /= experiment) $ H.modify_ _ { experiment = newExp }
        pure a

    render :: State -> H.ComponentHTML Query
    render s =
      HH.div_
        [HH.h1_ [ HH.text "Experiment"]]
