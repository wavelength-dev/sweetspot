module Supple.Component.Create where

import Prelude

import Data.Array (head)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (fromJust, Maybe(..))
import Data.Number (fromString)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Partial.Unsafe (unsafePartial)
import Supple.Capability.Experiment (class ManageExperiments, createExperiment)
import Supple.Component.Select as Select
import Supple.Component.TextField as TextField
import Supple.Component.Util (css)
import Supple.Data.Api (Products)

type State =
  { products :: Products
  , expName :: String
  , productId :: Number
  , price :: Number }

data Query a
  = UpdateProducts Products a
  | UpdateName TextField.Message a
  | UpdateProduct Select.Message a
  | UpdatePrice TextField.Message a
  | Submit a

type ChildSlot = Either3 Unit Unit Unit

type ChildQuery = Coproduct3 TextField.Query Select.Query TextField.Query

component
  :: forall m
   . ManageExperiments m =>
     H.Component HH.HTML Query Products Void m
component =
  H.parentComponent
    { initialState: \ps ->
       { products: ps
       , expName: ""
       , productId: unsafePartial ((_.id <<< fromJust <<< head) ps)
       , price: 0.0 }
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

      UpdateName (TextField.Value val) a -> do
        { expName } <- H.get
        when (val /= expName) $ H.modify_ _ { expName = val }
        pure a

      UpdateProduct (Select.Value val) a -> do
        { productId } <- H.get
        let parsed = fromString val
        case parsed of
          Just n -> do when (n /= productId) $ H.modify_ _ { productId = n }
          Nothing -> pure unit
        pure a

      UpdatePrice (TextField.Value val) a -> do
        { price } <- H.get
        let parsed = fromString val
        case parsed of
          Just p -> when (p /= price) $ H.modify_ _ { price = p }
          Nothing -> pure unit
        pure a

      Submit a -> do
       { price, productId, expName } <- H.get
       createExperiment { price, productId, name: expName }
       pure a

    toOpts :: Products -> Array Select.Option
    toOpts ps =
      (\{ title, id } -> { label: title, value: show id }) <$> ps

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { products, expName, productId } =
      HH.div
        [ css "Polaris-Page" ]
        [ HH.div
          [ css "Polaris-Page__Title" ]
          [ HH.h1
            [ css "Polaris-DisplayText Polaris-DisplayText--sizeLarge" ]
            [ HH.text "Create Experiment" ]]

        , HH.slot' CP.cp1 unit TextField.component "Experiment name" (HE.input UpdateName)

        , HH.slot' CP.cp2 unit Select.component { label: "Product", options: toOpts products } (HE.input UpdateProduct)

        , HH.slot' CP.cp3 unit TextField.component "Price" (HE.input UpdatePrice)

        , HH.button
          [ HE.onClick (HE.input_ Submit) ]
          [ HH.text "Create" ]]
