module SweetSpot.Data.Shopify where

type Price = Number
type ProductId = Number
type VariantId = Number

type Variant =
  { id :: VariantId
  , sku :: String
  , price :: Price
  }

type Product =
  { id :: ProductId
  , variants :: Array Variant
  }
