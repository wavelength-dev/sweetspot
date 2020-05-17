module Spec.Price
  ( priceSpec,
  )
where

import RIO
import SweetSpot.Data.Common
  ( FormattedPrice (..),
    MoneyFormat (..),
    Price (..),
  )
import SweetSpot.Util (formatPrice)
import Test.Hspec

amount = MoneyFormat "${{amount}}"

amountNoDecimals = MoneyFormat "${{amount_no_decimals}}"

amountWithCommaSep = MoneyFormat "${{amount_with_comma_separator}}"

amountNoDecimalsWithCommaSep = MoneyFormat "${{amount_no_decimals_with_comma_separator}}"

amountWithApostropheSeparator = MoneyFormat "{{amount_with_apostrophe_separator}} €"

priceSpec :: Spec
priceSpec = do
  describe "Price formatting" $ do
    it "should format {{amount}} correctly" $
      formatPrice amount (Price 10.9) `shouldBe` FormattedPrice "$10.90"
    it "should format {{amount}} correctly 2" $
      formatPrice amount (Price 1) `shouldBe` FormattedPrice "$1.00"
    it "{{amount}} should add .00 to integer prices" $
      formatPrice amount (Price 1000) `shouldBe` FormattedPrice "$1,000.00"
    it "{{amount_no_decimals}} should round up from .50" $
      formatPrice amountNoDecimals (Price 19.50) `shouldBe` FormattedPrice "$20"
    it "{{amount_no_decimals}} should round down from .49" $
      formatPrice amountNoDecimals (Price 19.49) `shouldBe` FormattedPrice "$19"
    it "{{amount_with_comma_separator}} should use comma for decimals" $
      formatPrice amountWithCommaSep (Price 100999.90)
        `shouldBe` FormattedPrice "$100.999,90"
    it "{{amount_no_decimals_comma_separator}} should round" $
      formatPrice amountNoDecimalsWithCommaSep (Price 100999.90)
        `shouldBe` FormattedPrice "$101.000"
    it "{{amount_with_apostrophe_separator}} should format correctly" $
      formatPrice amountWithApostropheSeparator (Price 100000000.90)
        `shouldBe` FormattedPrice "100'000'000.90 €"
