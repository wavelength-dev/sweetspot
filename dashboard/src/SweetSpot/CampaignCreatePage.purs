module SweetSpot.CampaignCreatePage where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Lens (view, (^.), over, traversed, filtered, (^..), folded)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Nullable (notNull, null)
import Data.Number (fromString) as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Effect.Uncurried (mkEffectFn1)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, JSX, component, element, useState')
import React.Basic.Hooks as React
import Routing.Hash as Hash
import SweetSpot.Data.Api (CreateCampaign(..), CreateExperiment(..), CreateVariant(..), Product, productId, productVariants, variantId, variantPrice, variantProductId, variantProductTitle, variantSku, variantTitle)
import SweetSpot.Service (makeCampaign)
import SweetSpot.Session (SessionId)
import SweetSpot.Shopify as Shopify
import SweetSpot.ShopifyHelper (formLayout) as SH
import SweetSpot.Spacing (large) as Spacing
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

foreign import styles :: forall a. Record a

type ProductPickerProps
  = { onProductSelected :: Product -> Effect Unit, products :: Array Product, show :: Boolean }

mkProductPicker :: Component ProductPickerProps
mkProductPicker =
  component "ProductPicker" \props -> React.do
    pure mempty

productToOptions :: Product -> Array { value :: String, label :: String }
productToOptions product = Maybe.fromMaybe [] options
  where
  options = product ^. productVariants # Array.head <#> variantToOption >>> Array.singleton

  skus = product ^.. productVariants <<< folded <<< variantSku

  variantToOption variant =
    let
      id = variant ^. variantId

      title = variant ^. variantProductTitle

      sku = variant ^. variantSku

      price = variant ^. variantPrice

      label = "title: " <> title <> ", " <> "sku: " <> Array.intercalate ", " skus
    in
      { value: id, label }

type VariantRow
  = { title :: String
    , id :: String
    , sku :: String
    , controlPrice :: String
    , testPrice :: String
    , testPriceValidation :: ParsedPrice
    , productId :: String
    }

productToVariantRows :: Product -> Array VariantRow
productToVariantRows product =
  map
    ( \variant ->
        let
          id = variant ^. variantId

          title = variant ^. variantTitle

          sku = variant ^. variantSku

          price = variant ^. variantPrice

          productId = variant ^. variantProductId
        in
          { id
          , title
          , sku
          , controlPrice: price
          , testPrice: ""
          , testPriceValidation: Initial
          , productId
          }
    )
    (product ^. productVariants)

renderVariantRow :: (String -> Effect Unit) -> VariantRow -> JSX
renderVariantRow onTestPriceChange { title, sku, controlPrice, testPrice, testPriceValidation } =
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
            , type: "number"
            , error:
                case testPriceValidation of
                  Empty -> notNull "Must enter a test price"
                  ContainsComma -> notNull "Use period symbol '.' to delimit cents"
                  ContainsCurrencySymbol -> notNull "Amounts are always in $, remove currency sign"
                  OtherIssue -> notNull "Invalid amount, example: 34.99"
                  Initial -> null
                  ValidPrice number -> null
            }
        ]
    ]

variantRowsToCreateExperiment :: Array VariantRow -> Array CreateExperiment
variantRowsToCreateExperiment =
  Array.sortBy compareProductId
    >>> Array.groupBy (\a b -> a.productId == b.productId)
    >>> map toCreateExperiment
  where
  compareProductId :: VariantRow -> VariantRow -> Ordering
  compareProductId a b = compare a.productId b.productId

  toCreateExperiment :: NonEmptyArray VariantRow -> CreateExperiment
  toCreateExperiment grouped =
    CreateExperiment
      { _createExperimentProductId: grouped # NEA.head >>> _.productId
      , _createExperimentVariants:
          grouped
            # NEA.mapMaybe
                ( \cv -> case cv.testPriceValidation of
                    ValidPrice price ->
                      Just
                        ( CreateVariant
                            { _createVariantSvid: cv.id
                            , _createVariantPrice: price
                            }
                        )
                    _ -> Nothing
                )
      }

data ParsedPrice
  = ValidPrice Number
  | ContainsComma
  | ContainsCurrencySymbol
  | Empty
  | OtherIssue
  | Initial

isValidPrice :: ParsedPrice -> Boolean
isValidPrice (ValidPrice _) = true

isValidPrice _ = false

containsComma :: String -> Boolean
containsComma = String.contains (Pattern ",")

containsCurrencySymbol :: String -> Boolean
containsCurrencySymbol input
  | String.contains (Pattern "$") input = true
  | String.contains (Pattern "€") input = true
  | otherwise = false

parseTestPrice :: String -> ParsedPrice
parseTestPrice testPrice
  | containsComma testPrice = ContainsComma
  | containsCurrencySymbol testPrice = ContainsCurrencySymbol
  | String.null testPrice = Empty
  | otherwise = case Number.fromString testPrice of
    Nothing -> OtherIssue
    Just number -> ValidPrice number

mkCampaignCreatePage :: Component { products :: Array Product, sessionId :: SessionId }
mkCampaignCreatePage = do
  now <- nowDateTime
  component "CampaignCreatePage" \props -> React.do
    name /\ setName <- useState' ""
    isProductPickerVisible /\ setIsProductPickerVisible <- useState' false
    -- Controlled by Shopify OptionList, tracks which variants the user would like to test
    variantRows /\ setVariantRows <- useState' ([] :: Array VariantRow)
    loading /\ setLoading <- useState' false
    modalOpen /\ setModalOpen <- useState' false
    let
      createCampaign :: CreateCampaign
      createCampaign =
        CreateCampaign
          { _createCampaignName: name
          , _createCampaignEnd: Nothing
          , _createCampaignExperiments: variantRowsToCreateExperiment variantRows
          }

      onNameChange = mkEffectFn1 setName

      onSubmit =
        mkEffectFn1
          ( \_ ->
              liftEffect (setLoading true)
                *> makeCampaign props.sessionId createCampaign
                *> liftEffect
                    ( setLoading false
                        *> Hash.setHash "/"
                        *> window
                        >>= location
                        >>= reload
                    )
                # Aff.launchAff_
          )

      unsafeGetProductById :: String -> Product
      unsafeGetProductById id = unsafePartial $ Array.unsafeIndex props.products 0

      -- Updates the variant rows to match the selected variants.
      -- We can't recreate variant rows because existing ones might have a modified test price.
      updateVariantRowsWithSelected :: Array Product -> Array VariantRow
      updateVariantRowsWithSelected = Array.concatMap createNewVariantRow
        where
        getExistingVariantRow id = Array.find (_.id >>> eq id) variantRows

        createNewVariantRow = productToVariantRows

      -- Takes a new list of variants to test and updates our variant rows
      onSelectedVariantsUpdated :: Array String -> Effect Unit
      onSelectedVariantsUpdated newVariantIdsToTest = (updateVariantRowsWithSelected products # setVariantRows)
        where
        products = Array.filter (view productId >>> flip Array.elem newVariantIdsToTest) props.products

      -- Takes a new test price and updates the variant rows with a new row with the new test price
      updateVariantRowsWithTestPrice :: VariantRow -> String -> Array VariantRow
      updateVariantRowsWithTestPrice targetVariantRow newPrice =
        over
          (traversed <<< filtered (_.id >>> eq targetVariantRow.id))
          (_ { testPrice = newPrice, testPriceValidation = parseTestPrice newPrice })
          variantRows

      mkSetVariantTestPrice :: VariantRow -> String -> Effect Unit
      mkSetVariantTestPrice targetVariantRow newPrice = updateVariantRowsWithTestPrice targetVariantRow newPrice # setVariantRows

      selectedVariantIds :: Array String
      selectedVariantIds = map _.id variantRows

      isValidCreateCampaign :: Boolean
      isValidCreateCampaign = Array.all (_.testPriceValidation >>> isValidPrice) variantRows
    pure
      $ element Shopify.page
          { title: notNull "Create experiment"
          , subtitle: null
          , breadcrumbs: [ { content: "Campaign list", url: "#/" } ]
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
                                ]
                            }
                      )
                  }
              , element Shopify.form
                  { onSubmit: mkEffectFn1 (const mempty)
                  , children:
                      [ Spacing.large
                      , SH.formLayout
                          [ element Shopify.textField
                              { value: name
                              , onChange: onNameChange
                              , label: "Experiment name"
                              , labelHidden: false
                              , children: mempty
                              , error: null
                              , type: "text"
                              }
                          , Spacing.large
                          , element Shopify.button
                              { onClick: notNull $ mkEffectFn1 $ const $ setIsProductPickerVisible true
                              , children: [ R.text "Select products" ]
                              , primary: false
                              , submit: false
                              , url: null
                              , loading: false
                              , disabled: false
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
                              { submit: false
                              , primary: true
                              , children: [ R.text "Create Experiment" ]
                              , url: null
                              , onClick: const (setModalOpen true) # mkEffectFn1 >>> notNull
                              , loading: false
                              , disabled: not isValidCreateCampaign || Array.null variantRows || modalOpen
                              }
                          ]
                      ]
                  }
              , element Shopify.modal
                  { title: "Are you sure you want to create this experiment?"
                  , open: modalOpen
                  , onClose: const (setModalOpen false) # mkEffectFn1
                  , children:
                      [ element Shopify.modalSection
                          { key: "confirmation-section"
                          , children:
                              [ R.p
                                  { className: styles.confirmText
                                  , children: [ R.text "Confirming will start the experiment immediately." ]
                                  }
                              , element Shopify.button
                                  { submit: true
                                  , primary: true
                                  , children: [ R.text "Confirm" ]
                                  , url: null
                                  , onClick: notNull onSubmit
                                  , loading: loading
                                  , disabled: false
                                  }
                              ]
                          }
                      ]
                  }
              ]
          }
