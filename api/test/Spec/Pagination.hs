module Spec.Pagination
  ( paginationSpec,
  )
where

import RIO
import SweetSpot.Shopify.Pagination
  ( LinkHeader (..),
    PageInfo (..),
    Pagination (..),
    parseLinkHeader,
  )
import Test.Hspec

both :: LinkHeader
both = LinkHeader "<https://libertyprice.myshopify.com/admin/api/2020-04/products.json?page_info=lol123&limit=10>; rel=\"next\", <https://libertyprice.myshopify.com/admin/api/2020-04/products.json?page_info=bal123&limit=10>; rel=\"previous\""

onlyNext :: LinkHeader
onlyNext = LinkHeader "<https://libertyprice.myshopify.com/admin/api/2020-04/products.json?limit=10&page_info=lol123>; rel=\"next\""

onlyPrev :: LinkHeader
onlyPrev = LinkHeader "<https://libertyprice.myshopify.com/admin/api/2020-04/products.json?page_info=bal123&limit=10>; rel=\"previous\""

paginationSpec :: Spec
paginationSpec = do
  describe "Link parsing" $ do
    it "Should parse both directions" $
      parseLinkHeader both
        `shouldBe` Pagination
          { _paginationNext = Just (PageInfo "lol123"),
            _paginationPrevious = Just (PageInfo "bal123")
          }
    it "Should parse only next" $
      parseLinkHeader onlyNext
        `shouldBe` Pagination
          { _paginationNext = Just (PageInfo "lol123"),
            _paginationPrevious = Nothing
          }
    it "Should parse only previous" $
      parseLinkHeader onlyPrev
        `shouldBe` Pagination
          { _paginationNext = Nothing,
            _paginationPrevious = Just (PageInfo "bal123")
          }
