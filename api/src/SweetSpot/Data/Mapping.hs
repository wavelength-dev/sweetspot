module SweetSpot.Data.Mapping
  ( fromShopProduct,
  )
where

import RIO
import qualified RIO.List as L
import SweetSpot.Data.Api
  ( Image (..),
    Product (..),
    Variant (..),
  )
import SweetSpot.Data.Common (MoneyFormat)
import SweetSpot.Shopify.Types
import SweetSpot.Util (formatPrice)

fromShopImage :: ShopImage -> Image
fromShopImage img =
  Image {_imageSrc = img ^. shopImageSrc}

fromShopVariant :: MoneyFormat -> ShopVariant -> Variant
fromShopVariant fmt v =
  Variant
    { _variantId = v ^. shopVariantId,
      _variantProductId = v ^. shopVariantProductId,
      _variantTitle = v ^. shopVariantTitle,
      _variantSku = v ^. shopVariantSku,
      _variantPrice = v ^. shopVariantPrice & formatPrice fmt
    }

fromShopProduct :: MoneyFormat -> ShopProduct -> Product
fromShopProduct fmt p =
  Product
    { _productId = p ^. shopProductId,
      _productTitle = p ^. shopProductTitle,
      _productImage = p ^. shopProductImage & fromShopImage,
      _productVariants = p ^. shopProductVariants & L.map (fromShopVariant fmt)
    }
