{-# LANGUAGE OverloadedStrings #-}

module SweetSpot.Shop where

import Data.Text as Text

data Shop = LibertyPrice | Longvadon
  deriving (Show)

data ShopConfig = ShopConfig { shopApi :: !Text
                             , accessTokenEndpoint :: !Text
                             , clientId :: !Text
                             }

readShop :: Text -> Either Text Shop
readShop "libertyprice" = Right LibertyPrice
readShop "longvadon"    = Right Longvadon
readShop unknownShop =
  Left
    $  "Trying to get ShopConfig for shop "
    <> unknownShop
    <> " which is not known."

getShopConfig :: Shop -> ShopConfig
getShopConfig LibertyPrice = ShopConfig
  { shopApi             = "https://libertyprice.myshopify.com/admin/api/2019-07"
  , accessTokenEndpoint =
    "https://libertyprice.myshopify.com/admin/oauth/access_token"
  , clientId            = "634b531a6568d6eb076c2ad5c7e0265a"
  }
getShopConfig Longvadon = ShopConfig
  { shopApi             = "https://monvadon.myshopify.com/admin/api/2019-07"
  , accessTokenEndpoint =
    "https://monvadon.myshopify.com/admin/oauth/access_token"
  , clientId            = "fdd642de7f5db1db727474ea0a4b9fd4"
  }
