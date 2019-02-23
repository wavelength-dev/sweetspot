module Supple.Component.Form.Experiment where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Supple.Capability.Experiment (class ManageExperiments, createExperiment)
import Supple.Component.Form.Button as Button
import Supple.Component.Form.Select as Select
import Supple.Component.Form.TextField as TextField
import Supple.Component.Form.Validation (Error, isGtZero, isNonEmpty, isNumber)
import Supple.Data.Api (Products)

newtype ExperimentForm r f = ExperimentForm (r
  ( name :: f Error String String
  , productId :: f Error String Number
  , price:: f Error String Number
  ))

derive instance newtypeForm :: Newtype (ExperimentForm r f) _

validators :: forall m. Monad m => ExperimentForm Record (F.Validation ExperimentForm m)
validators = ExperimentForm
  { name: isNonEmpty
  , productId: isNumber >>> isGtZero
  , price: isNumber >>> isGtZero
  }

renderFormlessWith
  :: forall m
   . MonadAff m
  => Products
  -> F.State ExperimentForm m
  -> F.HTML' ExperimentForm m
renderFormlessWith products =
  \fstate ->
    HH.div_
    [ TextField.component
        { placeholder: "Experiment name"
        , value: F.getInput _name fstate.form
        , onUpdate: HE.input $ F.setValidate _name
        }

    , Select.component
        { label: "Product"
        , value: F.getInput _productId fstate.form
        , options: toOpts products
        , onUpdate: HE.input $ F.setValidate _productId
        }

    , TextField.component
        { placeholder: "Price"
        , value: F.getInput _price fstate.form
        , onUpdate: HE.input $ F.asyncSetValidate debounceTime _price
        }

    , Button.component
        { label: "Submit"
        , onClick: HE.input_ F.submit
        }
    ]
    where
      _name = SProxy :: SProxy "name"
      _productId = SProxy :: SProxy "productId"
      _price = SProxy :: SProxy "price"
      debounceTime = Milliseconds 300.0

type State =
  { products :: Products }

data Query a
  = HandleForm (F.Message' ExperimentForm) a

type Input = Products

type ChildQuery m = F.Query' ExperimentForm m

type ChildSlot = Unit

type Message = Unit

component
  :: forall m
   . ManageExperiments m
  => MonadAff m
  => H.Component HH.HTML Query Input Void m
component = H.parentComponent
  { initialState: \ps -> { products: ps }
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ParentHTML Query (ChildQuery m) ChildSlot m
  render { products } =
    HH.div_
    [ HH.slot unit F.component
        { initialInputs, validators, render: renderFormlessWith products }
        (HE.input HandleForm)
    ]
    where
      defaultPid = case head products of
        Just p -> show $ _.id p
        Nothing -> ""
      initialInputs :: ExperimentForm Record F.InputField
      initialInputs = F.wrapInputFields { name: "", productId: defaultPid, price: "" }


  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Void m
  eval (HandleForm m a) = case m of
    F.Submitted formOutput -> a <$ do
      let experiment = F.unwrapOutputFields formOutput
      createExperiment experiment

    -- TODO implement
    F.Changed  form -> pure a
    F.Emit form -> pure a


toOpts :: Products -> Array Select.Option
toOpts ps = (\p -> { value: show p.id, label: p.title }) <$> ps
