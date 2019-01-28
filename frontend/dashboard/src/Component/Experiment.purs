module Component.Experiment where

import Prelude

import Halogen.HTML as HH
import AppM (Env)
import Capability.Navigate (class Navigate)
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H

type State = Maybe Int

data Query a = Initialize a

component
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const Nothing
    , render
    , eval
    , receiver: const Nothing
    }
  where

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      Initialize a -> do
        pure a

    render :: State -> H.ComponentHTML Query
    render _ =
      HH.div_
        [HH.h1_ [ HH.text "Experiment"]]
