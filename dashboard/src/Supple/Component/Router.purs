module Supple.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array (find)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Partial.Unsafe (unsafePartial)
import Supple.AppM (Env)
import Supple.Capability.Experiment (class ManageExperiments, getExperiments, getProducts)
import Supple.Capability.Navigate (class Navigate)
import Supple.Component.Experiment as Experiment
import Supple.Component.Home as Home
import Supple.Component.Create as Create
import Supple.Component.Loading as Loading
import Supple.Data.Api (Experiment, Experiments, ExperimentsResource, ProductsResource)
import Supple.Data.Route (Route(..))

type State =
  { route :: Route
  , experiments :: ExperimentsResource
  , products :: ProductsResource }

type Input = Maybe Route

data Query a
  = Initialize a
  | Navigate Route a

type ChildQuery = Coproduct4 Home.Query Experiment.Query Create.Query Loading.Query

type ChildSlot = Either4 Unit Unit Unit Unit

component
  :: forall m
     . MonadAff m
    => MonadAsk Env m
    => Navigate m
    => ManageExperiments m
    => H.Component HH.HTML Query Input Void m
component =
  H.lifecycleParentComponent
    { initialState: \initialRoute ->
       { route: fromMaybe Home initialRoute
       , experiments: Nothing
       , products: Nothing }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      Initialize a -> do
        exps <- getExperiments
        prods <- getProducts
        H.modify_ _ { experiments = exps, products = prods }
        pure a

      (Navigate dest a) -> do
        { route } <- H.get
        when (route /= dest) do
          H.modify_ _ { route = dest }
        pure a

    getExperiment :: Int -> Experiments -> Experiment
    getExperiment expId exps = unsafePartial $ fromJust $ (find (\e -> e.exp_id == expId) exps)

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state = case state of
      { route: Home, experiments: (Just exps) } ->
        HH.slot' CP.cp1 unit Home.component exps absurd

      { route: Experiment expId, experiments: (Just exps) } ->
        HH.slot' CP.cp2 unit Experiment.component (getExperiment expId exps) absurd

      { route: Create, products: (Just prods) } ->
        HH.slot' CP.cp3 unit Create.component prods absurd

      _ -> HH.slot' CP.cp4 unit Loading.component unit absurd
