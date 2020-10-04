module SweetSpot.ProductsResource where

import SweetSpot.Data.Api (Product)

data ProductsResource
  = EmptyProducts
  | PartialProducts (Array Product)
  | AllProducts (Array Product)
  | FailedFetch
