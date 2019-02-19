module Supple.Component.Form.Experiment where

import Prelude

import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Supple.Capability.Experiment (class ManageExperiments, createExperiment)
import Supple.Component.Form.TextField as TextField
import Supple.Component.Form.Validation (Error, isGtZero, isNonEmpty, isNumber)

newtype ExperimentForm r f = ExperimentForm (r
  ( name :: f Error String String
  , productId :: f Error String Number
  , price:: f Error String Number
  ))

derive instance newtypeForm :: Newtype (ExperimentForm r f) _

inputs :: ExperimentForm Record F.InputField
inputs = F.wrapInputFields
  { name: ""
  , productId: ""
  , price: ""
  }

validators :: forall m. Monad m => ExperimentForm Record (F.Validation ExperimentForm m)
validators = ExperimentForm
  { name: isNonEmpty
  , productId: isNumber >>> isGtZero
  , price: isNumber >>> isGtZero
  }

initialInputs :: ExperimentForm Record F.InputField
initialInputs = F.wrapInputFields { name: "", productId: "", price: "" }

renderFormless
  :: forall m
   . MonadAff m
  => F.State ExperimentForm m
  -> F.HTML' ExperimentForm m
renderFormless fstate =
  HH.div_
  [ TextField.component "Experiment name"
    (F.getInput _name fstate.form)
    (HE.input $ F.setValidate _name)

  , TextField.component "Product id"
    (F.getInput _productId fstate.form)
    (HE.input $ F.asyncSetValidate debounceTime _productId)

  , TextField.component "Price"
    (F.getInput _productId fstate.form)
    (HE.input $ F.asyncSetValidate debounceTime _price)
  ]
  where
    _name = SProxy :: SProxy "name"
    _productId = SProxy :: SProxy "productId"
    _price = SProxy :: SProxy "price"
    debounceTime = Milliseconds 300.0

type State = Unit

data Query a
  = HandleForm (F.Message' ExperimentForm) a

type Input = Unit

type ChildQuery m = F.Query' ExperimentForm m

type ChildSlot = Unit

type Massage = Unit

component
  :: forall m
   . ManageExperiments m
  => MonadAff m
  => H.Component HH.HTML Query Input Void m
component = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render st =
    HH.div_
    [ HH.slot unit F.component
        { initialInputs, validators, render: renderFormless }
        (HE.input HandleForm)
    ]

  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Void m
  eval (HandleForm m a) = case m of
    F.Submitted formOutput -> a <$ do
      let experiment = F.unwrapOutputFields formOutput
      createExperiment experiment
      pure a

    _ -> pure a
