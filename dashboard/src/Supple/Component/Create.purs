module Supple.Component.Create where

import Prelude

import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Supple.Capability.Experiment (class ManageExperiments)
import Supple.Component.Form.Experiment as ExperimentForm
import Supple.Component.Util (css)
import Supple.Data.Api (Product)

type State =
  { products :: Array Product }

data Query a
  = UpdateProducts (Array Product) a

type ChildSlot = Either1 Unit

type ChildQuery = Coproduct1 ExperimentForm.Query

component
  :: forall m
   . ManageExperiments m
  => MonadAff m
  => H.Component HH.HTML Query (Array Product) Void m
component =
  H.parentComponent
    { initialState: \ps -> { products: ps }
    , render
    , eval
    , receiver: HE.input UpdateProducts
    }
  where

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      UpdateProducts newProds a -> do
        { products } <- H.get
        when (newProds /= products) $ H.modify_ _ { products = newProds }
        pure a

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { products } =
      HH.div
        [ css "Polaris-Page" ]
        [ HH.div
          [ css "Polaris-Page__Title" ]
          [ HH.h1
            [ css "Polaris-DisplayText Polaris-DisplayText--sizeLarge" ]
            [ HH.text "Create Experiment" ]]

        , HH.slot' CP.cp1 unit ExperimentForm.component products absurd
        ]
