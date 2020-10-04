module SweetSpot.CampaignCreatePage where

import Prelude
import Data.Array ((\\))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (or)
import Data.Lens (filtered, folded, over, traversed, view, (^.), (^..))
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Number (fromString) as Number
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff (attempt, delay, launchAff_) as Aff
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Effect.Uncurried (mkEffectFn1)
import Partial.Unsafe (unsafePartial)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, JSX, Render, UseState, component, element, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import Routing.Hash as Hash
import SweetSpot.Data.Api (CreateCampaign(..), CreateExperiment(..), CreateVariant(..), Product, Variant, pagination, paginationNext, productId, productTitle, productVariants, variantId, variantPrice, variantProductId, variantSku, variantTitle)
import SweetSpot.Data.Api (products) as SL
import SweetSpot.Logger (LogLevel(..)) as LogLevel
import SweetSpot.Logger as Logger
import SweetSpot.Mock (bonoboHat, bonoboHat2)
import SweetSpot.ProductsResource (ProductsResource(..))
import SweetSpot.Service (makeCampaign)
import SweetSpot.Service as Service
import SweetSpot.Session (SessionId)
import SweetSpot.Shopify as Shopify
import SweetSpot.ShopifyHelper (formLayout) as SH
import SweetSpot.Spacing (large, medium) as Spacing
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

productToOption :: Product -> { value :: String, label :: String }
productToOption product = { value, label }
  where
  value = product ^. productId

  title = product ^. productTitle

  skus = product ^.. productVariants <<< folded <<< variantSku

  label = "title: " <> title <> ", " <> "sku: " <> Array.intercalate ", " skus

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

          title = view productTitle product <> " - " <> view variantTitle variant

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
                  EmptyPrice -> notNull "Must enter a test price"
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
  | EmptyPrice
  | OtherIssue
  | Initial

derive instance eqParsedPrice :: Eq ParsedPrice

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
  | String.null testPrice = EmptyPrice
  | otherwise = case Number.fromString testPrice of
    Nothing -> OtherIssue
    Just number -> ValidPrice number

-- useProductsResource :: forall deps. SessionId -> Hook (UseAff deps Unit) ProductsResource
useProductsResource :: forall deps. SessionId -> Render deps (UseAff SessionId Unit (UseState ProductsResource deps)) ProductsResource
useProductsResource sessionId = React.do
  productsResource /\ setProductsResource <- useState' EmptyProducts
  let
    fetchMoreProducts :: Array Product -> Maybe String -> Aff Unit
    -- last fetch
    fetchMoreProducts previouslyFetched Nothing = setProductsResource (AllProducts previouslyFetched) # liftEffect

    -- recursing fetch
    fetchMoreProducts previouslyFetched (Just nextPageInfo) = do
      setProductsResource (PartialProducts previouslyFetched) # liftEffect
      ePaginatedProducts <- Service.fetchProducts sessionId (Just nextPageInfo) # Aff.attempt
      case ePaginatedProducts of
        Left error ->
          liftEffect do
            setProductsResource FailedFetch
            Logger.logWithContext LogLevel.Error "failed to fetch products" error
        Right paginatedProducts -> do
          let
            mNextPage = paginatedProducts ^. pagination ^. paginationNext

            fetchedProducts = paginatedProducts ^. SL.products
          fetchMoreProducts (previouslyFetched <> fetchedProducts) mNextPage
  useAff sessionId do
    ePaginatedProducts <- Service.fetchProducts sessionId Nothing # Aff.attempt
    case ePaginatedProducts of
      Left error ->
        liftEffect do
          setProductsResource FailedFetch
          Logger.logWithContext LogLevel.Error "failed to fetch products" error
      Right paginatedProducts -> do
        let
          mNextPage = paginatedProducts ^. pagination ^. paginationNext

          fetchedProducts = paginatedProducts ^. SL.products
        fetchMoreProducts ([] <> fetchedProducts) mNextPage
    setProductsResource (PartialProducts []) # liftEffect
    Aff.delay (Milliseconds 3000.0)
    setProductsResource (PartialProducts [ bonoboHat ]) # liftEffect
    Aff.delay (Milliseconds 3000.0)
    setProductsResource (AllProducts [ bonoboHat, bonoboHat2 ]) # liftEffect
  pure productsResource

mkCampaignCreatePage :: Component { sessionId :: SessionId }
mkCampaignCreatePage = do
  now <- nowDateTime
  component "CampaignCreatePage" \props -> React.do
    name /\ setName <- useState' ""
    isProductPickerVisible /\ setIsProductPickerVisible <- useState' false
    variantRows /\ setVariantRows <- useState' ([] :: Array VariantRow)
    loading /\ setLoading <- useState' false
    modalOpen /\ setModalOpen <- useState' false
    -- Controlled by Shopify OptionList, tracks which variants the user would like to test
    selectedRawVariantIds /\ setSelectedRawVariantIds <- useState' ([] :: Array String)
    productsResource <- useProductsResource props.sessionId
    productFilter /\ setProductFilter <- useState' ""
    let
      products = case productsResource of
        EmptyProducts -> []
        FailedFetch -> []
        PartialProducts fetchedProducts -> fetchedProducts
        AllProducts fetchedProducts -> fetchedProducts

      productOptions = case productFilter of
        "" -> map productToOption products
        -- TODO: Lens it up
        filter ->
          let
            matchPattern = String.toLower filter # Pattern

            checkMatchingVariant :: Variant -> Boolean
            checkMatchingVariant variant =
              String.contains matchPattern (variant ^. variantTitle)
                || String.contains matchPattern (variant ^. variantSku)

            checkMatchingProduct :: Product -> Boolean
            checkMatchingProduct product =
              String.contains matchPattern (product ^. productTitle)
                || (map checkMatchingVariant (product ^. productVariants) # or)

            containsPattern = String.contains matchPattern
          in
            Array.filter checkMatchingProduct products # map productToOption

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
      unsafeGetProductById id = unsafePartial $ Array.unsafeIndex products 0

      selectedVariantIds :: Array String
      selectedVariantIds = map _.productId variantRows

      -- Takes a new list of variants to test and updates our variant rows
      -- Don't touch existing rows.
      -- Filter removed ones.
      -- Append new ones.
      onSelectedVariantsUpdated :: Array String -> Effect Unit
      onSelectedVariantsUpdated newSelectedRawVariantIds = do
        setSelectedRawVariantIds newSelectedRawVariantIds
        setVariantRows updatedVariantRows
        where
        elem' = flip Array.elem

        rowIds = map _.productId variantRows # Array.nub

        idsToAdd = Array.difference newSelectedRawVariantIds rowIds

        idsToRemove = Array.difference rowIds newSelectedRawVariantIds

        rowsToRemove = Array.filter (_.productId >>> elem' idsToRemove) variantRows

        rowsToAdd =
          products
            # Array.filter (view productId >>> elem' idsToAdd)
            >>> Array.concatMap productToVariantRows

        updatedVariantRows = (variantRows \\ rowsToRemove) <> rowsToAdd

      -- Takes a new test price and updates the variant rows with a new row with the new test price
      updateVariantRowsWithTestPrice :: VariantRow -> String -> Array VariantRow
      updateVariantRowsWithTestPrice targetVariantRow newPrice =
        over
          (traversed <<< filtered (_.id >>> eq targetVariantRow.id))
          (_ { testPrice = newPrice, testPriceValidation = parseTestPrice newPrice })
          variantRows

      mkSetVariantTestPrice :: VariantRow -> String -> Effect Unit
      mkSetVariantTestPrice targetVariantRow newPrice = updateVariantRowsWithTestPrice targetVariantRow newPrice # setVariantRows

      isValidCreateCampaign :: Boolean
      isValidCreateCampaign = Array.all (_.testPriceValidation >>> isValidPrice) variantRows

      onCloseProductPicker =
        setIsProductPickerVisible false
          *> setProductFilter ""
          # const
          >>> mkEffectFn1
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
                  , onClose: onCloseProductPicker
                  , children:
                      [ element Shopify.modalSection
                          { key: "0"
                          , children:
                              [ element Shopify.textField
                                  { value: productFilter
                                  , onChange: mkEffectFn1 setProductFilter
                                  , label: "search"
                                  , labelHidden: false
                                  , children: []
                                  , error: null
                                  , type: "text"
                                  }
                              ]
                          }
                      , element Shopify.modalSection
                          { key: "1"
                          , children:
                              [ element Shopify.optionList
                                  { allowMultiple: true
                                  , onChange: mkEffectFn1 onSelectedVariantsUpdated
                                  , options: productOptions
                                  , selected: selectedRawVariantIds
                                  }
                              ]
                          }
                      , case productsResource of
                          PartialProducts _ ->
                            element Shopify.modalSection
                              { key: "2"
                              , children:
                                  [ R.div
                                      { className: styles.loadingAdditionalProducts
                                      , children: [ R.text "loading additional products", Spacing.medium, element Shopify.spinner {} ]
                                      }
                                  ]
                              }
                          _ -> mempty
                      ]
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
                          , R.div
                              { className: styles.selectProductsContainer
                              , children:
                                  [ element Shopify.button
                                      { onClick: notNull $ mkEffectFn1 $ const $ setIsProductPickerVisible true
                                      , children: [ R.text "Select products" ]
                                      , primary: false
                                      , submit: false
                                      , url: null
                                      , loading: false
                                      , disabled: false
                                      }
                                  , R.div
                                      { className: styles.eligibilityMessage
                                      , children: [ R.text "All variants of a product need to have an SKU for it to be eligible for an experiment." ]
                                      }
                                  ]
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
