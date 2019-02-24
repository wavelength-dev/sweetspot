module Supple.Component.Form.Experiment where

import Prelude

import Data.Array (find, head, insert)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (trace)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Supple.Capability.Experiment (class ManageExperiments, createExperiment)
import Supple.Component.Form.Button as Button
import Supple.Component.Form.Select as Select
import Supple.Component.Form.TextField as TextField
import Supple.Component.Form.Validation (Error, isGtZero, isNonEmpty, isNumber)
import Supple.Component.Polaris as P
import Supple.Component.Util (css)
import Data.Lens
import Supple.Data.Api

newtype ExperimentForm r f = ExperimentForm (r
  ( _ceName :: f Error String String
  , _ceProductId :: f Error String Number
  , _cePrice:: f Error String Number
  ))

derive instance newtypeForm :: Newtype (ExperimentForm r f) _

validators :: forall m. Monad m => ExperimentForm Record (F.Validation ExperimentForm m)
validators = ExperimentForm
  { _ceName: isNonEmpty
  , _ceProductId: isNumber >>> isGtZero
  , _cePrice: isNumber >>> isGtZero
  }

renderFormlessWith
  :: forall m
   . MonadAff m
  => Array Product
  -> F.State ExperimentForm m
  -> F.HTML' ExperimentForm m
renderFormlessWith products =
  \fstate ->
    let selectedProduct = case (F.getResult _productId fstate.form) of
          F.Success pid -> find (\p -> p ^. pId == pid) products
          _ -> Nothing
        productSelected = isJust selectedProduct
    in
    P.section
      [ case selectedProduct of
          Just p -> renderProductImage p
          Nothing -> HH.div_ []

      , P.card
          ""
          [ Select.component
              { label: "Product"
              , value: F.getInput _productId fstate.form
              , options: toOpts products
              , onUpdate: HE.input $ F.setValidate _productId
              }
          , HH.div
            [ css $ if not productSelected then "hidden" else ""]
            [ TextField.component
                { placeholder: "Experiment name"
                , value: F.getInput _name fstate.form
                , onUpdate: HE.input $ F.setValidate _name
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
          ]
      ]
      where
        _name = SProxy :: SProxy "_ceName"
        _productId = SProxy :: SProxy "_ceProductId"
        _price = SProxy :: SProxy "_cePrice"
        debounceTime = Milliseconds 300.0

type State =
  { products :: Array Product }

data Query a
  = HandleForm (F.Message' ExperimentForm) a

type Input = Array Product

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
    P.layout
      [ HH.slot unit F.component
          { initialInputs, validators, render: renderFormlessWith products }
          (HE.input HandleForm)
      ]
    where
      defaultPid = case head products of
        Just p -> show $ p ^. pId
        Nothing -> ""
      initialInputs :: ExperimentForm Record F.InputField
      initialInputs = F.wrapInputFields
        { _ceName: "", _ceProductId: defaultPid, _cePrice: "" }


  eval :: Query ~> H.ParentDSL State Query (ChildQuery m) ChildSlot Void m
  eval (HandleForm m a) = case m of
    F.Submitted formOutput -> a <$ do
      let experiment = F.unwrapOutputFields formOutput
      createExperiment $ CreateExperiment experiment

    -- TODO implement
    F.Changed  form -> pure a
    F.Emit form -> pure a


toOpts :: Array Product -> Array Select.Option
toOpts ps = pickFields <$> ps
  where
    pickFields = \p -> { label: p ^. pTitle, value: show (p ^. pId) }

renderProductImage :: forall i p. Product -> HH.HTML i p
renderProductImage p =
    P.card
      (p ^. pTitle)
      [ HH.div
        [ css "thumbnail" ]
        [ HH.img
          [ HP.src (p ^. pImage) ]
        ]
      ]
