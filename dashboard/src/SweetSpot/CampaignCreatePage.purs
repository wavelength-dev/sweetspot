module SweetSpot.CampaignCreatePage where

import Prelude

import Data.Array (find, foldMap, null) as Array
import Data.Array (find, mapWithIndex)
import Data.Lens (view, (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Nullable (notNull, null)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_) as Aff
import Effect.Now (nowDateTime)
import Effect.Uncurried (mkEffectFn1)
import Global (readFloat)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM (table, tbody_, td_, text, th_, thead_, tr_) as R
import React.Basic.Hooks (Component, JSX, component, element, useState')
import React.Basic.Hooks as React
import SweetSpot.Data.Api (CreateCampaign(..), CreateExperiment(..), Product, Variant, productVariants, variantId, variantPrice, variantProductId, variantSku, variantTitle)
import SweetSpot.Service (makeCampaign)
import SweetSpot.Session (SessionId)
import SweetSpot.Shopify (button, card, form, modal, modalSection, optionList, page, textField) as Shopify
import SweetSpot.ShopifyHelper (formLayout) as SH
import SweetSpot.Spacing (large) as Spacing

foreign import styles :: forall a. Record a

type ProductPickerProps
  = { onProductSelected :: Product -> Effect Unit, products :: Array Product, show :: Boolean }

mkProductPicker :: Component ProductPickerProps
mkProductPicker =
  component "ProductPicker" \props -> React.do
    pure mempty

productToOptions :: Product -> Array { value :: String, label :: String }
productToOptions product = map variantToOption (product ^. productVariants)
  where
  variantToOption variant =
    let
      id = variant ^. variantId

      title = variant ^. variantTitle

      sku = variant ^. variantSku

      price = variant ^. variantPrice

      label = "title: " <> title <> ", " <> "sku: " <> sku
    in
      { value: id, label }

type VariantRow
  = { title :: String
    , id :: String
    , sku :: String
    , controlPrice :: String
    , testPrice :: String
    , productId :: String
    }

variantToVariantRow :: Variant -> VariantRow
variantToVariantRow variant =
  let
    id = variant ^. variantId

    title = variant ^. variantTitle

    sku = variant ^. variantSku

    price = variant ^. variantPrice

    productId = variant ^. variantProductId
  in
    { title
    , id
    , sku
    , controlPrice: price
    , testPrice: fromMaybe price (String.stripPrefix (Pattern "$") price)
    , productId
    }

renderVariantRow :: (String -> Effect Unit) -> VariantRow -> JSX
renderVariantRow onTestPriceChange { title, sku, controlPrice, testPrice } =
  R.tr_
    [ R.td_ [ R.text title ]
    , R.td_ [ R.text sku ]
    , R.td_ [ R.text controlPrice ]
    , R.td_
        [ element Shopify.textField
            { label: "test price"
            , labelHidden: true
            , children: mempty
            , onChange: (mkEffectFn1 onTestPriceChange)
            , value: testPrice
            }
        ]
    ]

variantRowToCreateExperiment :: VariantRow -> CreateExperiment
variantRowToCreateExperiment variantRow =
  CreateExperiment
    { _createExperimentProductId: variantRow.productId
    , _createExperimentPrice: readFloat variantRow.testPrice
    }

mkCampaignCreatePage :: Component { products :: Array Product, sessionId :: SessionId }
mkCampaignCreatePage = do
  now <- nowDateTime
  component "CampaignCreatePage" \props -> React.do
    name /\ setName <- useState' ""
    isProductPickerVisible /\ setIsProductPickerVisible <- useState' false
    -- Controlled by Shopify OptionList, tracks which variants the user would like to test
    variantRows /\ setVariantRows <- useState' ([] :: Array VariantRow)
    let
      createCampaign :: CreateCampaign
      createCampaign =
        CreateCampaign
          { _createCampaignName: name
          , _createCampaignEnd: Nothing
          , _createCampaignExperiments: map variantRowToCreateExperiment variantRows
          }

      onNameChange = mkEffectFn1 setName

      onSubmit = mkEffectFn1 (\_ -> Aff.launchAff_ (makeCampaign props.sessionId createCampaign))

      unsafeGetVariantById :: String -> Variant
      unsafeGetVariantById id = find (unwrap >>> _._variantId >>> eq id) variants # unsafePartial Maybe.fromJust
        where
        variants = Array.foldMap (view productVariants) props.products

      -- Updates the variant rows to match the selected variants.
      -- We can't recreate variant rows because existing ones might have a modified test price.
      updateVariantRowsWithSelected :: Array String -> Array VariantRow
      updateVariantRowsWithSelected = map \variantId -> Maybe.fromMaybe (createNewVariantRow (unsafeGetVariantById variantId)) (getExistingVariantRow variantId)
        where
        getExistingVariantRow id = Array.find (\vr -> vr.id == id) variantRows

        createNewVariantRow = variantToVariantRow

      -- Takes a new list of variants to test and updates our variant rows
      onSelectedVariantsUpdated :: Array String -> Effect Unit
      onSelectedVariantsUpdated newVariantIdsToTest = (updateVariantRowsWithSelected newVariantIdsToTest # setVariantRows)

      -- Takes a new test price and updates the variant rows with a new row with the new test price
      -- TODO: use lens
      updateVariantRowsWithTestPrice :: VariantRow -> String -> Array VariantRow
      updateVariantRowsWithTestPrice targetVariantRow newPrice = map (\vr -> if vr.id == targetVariantRow.id then vr { testPrice = newPrice } else vr) variantRows

      mkSetVariantTestPrice :: VariantRow -> String -> Effect Unit
      mkSetVariantTestPrice targetVariantRow newPrice = updateVariantRowsWithTestPrice targetVariantRow newPrice # setVariantRows

      selectedVariantIds :: Array String
      selectedVariantIds = map (\vr -> vr.id) variantRows
    pure
      $ element Shopify.page
          { title: notNull "Create experiment"
          , subtitle: null
          , breadcrumbs: [ { content: "campaign list", url: "#/" } ]
          , primaryAction: null
          , children:
              [ element Shopify.modal
                  { open: isProductPickerVisible
                  , title: "Products to test"
                  , onClose: mkEffectFn1 $ const $ setIsProductPickerVisible false
                  , children:
                      ( (flip mapWithIndex) props.products \i product ->
                          element Shopify.modalSection
                            { key: show i
                            , children:
                                [ element Shopify.optionList
                                    { onChange: mkEffectFn1 onSelectedVariantsUpdated
                                    , options: productToOptions product
                                    , selected: selectedVariantIds
                                    , allowMultiple: true
                                    }
                                , Spacing.large
                                , element Shopify.button
                                    { primary: true
                                    , children: [ R.text "Done" ]
                                    , onClick: notNull $ mkEffectFn1 $ const $ setIsProductPickerVisible false
                                    , url: null
                                    , submit: false
                                    }
                                ]
                            }
                      )
                  }
              , element Shopify.form
                  { onSubmit: onSubmit
                  , children:
                      [ Spacing.large
                      , SH.formLayout
                          [ element Shopify.textField
                              { value: name
                              , onChange: onNameChange
                              , label: "Experiment name"
                              , labelHidden: false
                              , children: mempty
                              }
                          , Spacing.large
                          , element Shopify.button
                              { onClick: notNull $ mkEffectFn1 $ const $ setIsProductPickerVisible true
                              , children: [ R.text "Select products" ]
                              , primary: false
                              , submit: false
                              , url: null
                              }
                          , element Shopify.card
                              { title: "Products to test"
                              , sectioned: true
                              , children:
                                  if (Array.null variantRows) then
                                    [ R.text "No products selected" ]
                                  else
                                    [ R.table
                                        { className: styles.productTable
                                        , children:
                                            [ R.thead_
                                                [ R.tr_
                                                    [ R.th_ [ R.text "Name" ]
                                                    , R.th_ [ R.text "Sku" ]
                                                    , R.th_ [ R.text "Control Price" ]
                                                    , R.th_ [ R.text "Test Price" ]
                                                    ]
                                                ]
                                            , R.tbody_
                                                ( map
                                                    ( \variantRow ->
                                                        renderVariantRow
                                                          (mkSetVariantTestPrice variantRow)
                                                          variantRow
                                                    )
                                                    variantRows
                                                )
                                            ]
                                        }
                                    ]
                              }
                          , Spacing.large
                          , element Shopify.button
                              { submit: true
                              , primary: true
                              , children: [ R.text "Create Experiment" ]
                              , url: null
                              , onClick: null
                              }
                          ]
                      ]
                  }
              ]
          }
