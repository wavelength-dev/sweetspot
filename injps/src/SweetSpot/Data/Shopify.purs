module SweetSpot.Data.Shopify where

type Price = Number
type ProductId = Number
type VariantId = Number
type Sku = String

type Variant =
  { id :: VariantId
  , sku :: Sku
  , price :: Price
  }

type Product =
  { id :: ProductId
  , variants :: Array Variant
  }
