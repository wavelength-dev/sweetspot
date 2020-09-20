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

fromShopImage :: Maybe ShopImage -> Maybe Image
fromShopImage = fmap (\img -> Image {_imageSrc = img ^. shopImageSrc})

fromShopVariant :: Text -> MoneyFormat -> ShopVariant -> Maybe Variant
fromShopVariant productTitle fmt v =
  ( \sku ->
      Variant
        { _variantId = v ^. shopVariantId,
          _variantProductId = v ^. shopVariantProductId,
          _variantTitle = v ^. shopVariantTitle,
          _variantProductTitle = productTitle,
          _variantSku = sku,
          _variantPrice = v ^. shopVariantPrice & formatPrice fmt
        }
  )
    <$> view shopVariantSku v

fromShopProduct :: MoneyFormat -> ShopProduct -> Maybe Product
fromShopProduct fmt p =
  view shopProductVariants p
    & ( L.map (fromShopVariant title fmt)
          >>> sequence
          >>> fmap
            ( \variants ->
                Product
                  { _productId = p ^. shopProductId,
                    _productTitle = title,
                    _productImage = p ^. shopProductImage & fromShopImage,
                    _productVariants = variants
                  }
            )
      )
  where
    title = p ^. shopProductTitle
