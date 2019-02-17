module Supple.Component.Experiment where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Supple.AppM (Env)
import Supple.Data.Api (Experiment)
import Supple.Component.Util (css)

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
    render { experiment } =
      HH.div
        [ css "Polaris-Page" ]
        [HH.div
         [ css "Polaris-Page__Title"]
         [HH.h1
          [css "Polaris-DisplayText Polaris-DisplayText--sizeLarge"]
          [ HH.text "Experiment"]]
        , HH.text experiment.name]
