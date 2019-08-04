module SweetSpot.Data.Product where

import Prelude

newtype Sku
  = Sku String

instance showSku :: Show Sku where
  show sku = show sku
