module SweetSpot.Data.Shopify where

type Variant =
  { id :: Number
  , sku :: String
  , price :: Number
  }

type Product =
  { id :: Number
  , variants :: Array Variant
  }
