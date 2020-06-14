module SweetSpot.Util
  ( nanToNothing,
    nanToZero,
    formatPrice,
  )
where

import Data.Scientific
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import SweetSpot.Data.Common

nanToNothing :: Double -> Maybe Double
nanToNothing double = if isNaN double then Nothing else Just double

nanToZero :: Double -> Double
nanToZero double = if isNaN double then 0 else double

data ThousandSep = Point Bool | Comma Bool | Apostrophe

formatPrice :: MoneyFormat -> Price -> FormattedPrice
formatPrice (MoneyFormat format) (Price price)
  | T.isInfixOf amount format =
    let formatted = withFormat (Point True) price
     in FormattedPrice $ T.replace amount formatted format
  | T.isInfixOf amountNoDecimals format =
    let formatted = withFormat (Point False) price
     in FormattedPrice $ T.replace amountNoDecimals formatted format
  | T.isInfixOf amountWithCommaSep format =
    let formatted = withFormat (Comma True) price
     in FormattedPrice $ T.replace amountWithCommaSep formatted format
  | T.isInfixOf amountNoDecimalsWithCommaSep format =
    let formatted = withFormat (Comma False) price
     in FormattedPrice $ T.replace amountNoDecimalsWithCommaSep formatted format
  | T.isInfixOf amountWithApostropheSep format =
    let formatted = withFormat Apostrophe price
     in FormattedPrice $ T.replace amountWithApostropheSep formatted format
  | otherwise = error "Invalid money format"
  where
    amount = "{{amount}}"
    amountNoDecimals = "{{amount_no_decimals}}"
    amountWithCommaSep = "{{amount_with_comma_separator}}"
    amountNoDecimalsWithCommaSep = "{{amount_no_decimals_with_comma_separator}}"
    amountWithApostropheSep = "{{amount_with_apostrophe_separator}}"

withFormat :: ThousandSep -> Scientific -> Text
withFormat (Point True) n =
  toText True n
    & splitDecimals
    & first (addThousandSep ",")
    & joinWithDecimalSep "."
withFormat (Point False) n =
  toText False n
    & addThousandSep ","
withFormat (Comma True) n =
  toText True n
    & splitDecimals
    & first (addThousandSep ".")
    & joinWithDecimalSep ","
withFormat (Comma False) n =
  toText False n
    & addThousandSep "."
withFormat Apostrophe n =
  toText True n
    & splitDecimals
    & first (addThousandSep "'")
    & joinWithDecimalSep "."

toText :: Bool -> Scientific -> Text
toText True n = T.pack $ formatScientific Fixed (Just 2) n
toText False n = T.pack $ formatScientific Fixed (Just 0) (fromIntegral $ round n)

addThousandSep :: Text -> Text -> Text
addThousandSep sep wholes =
  wholes
    & T.reverse
    & T.chunksOf 3
    & L.map T.reverse
    & L.reverse
    & T.intercalate sep

splitDecimals :: Text -> (Text, Text)
splitDecimals p = case T.split (== '.') p of
  [wholes, decimals] -> (wholes, decimals)
  _ -> error "Tried to split decimals for invalid price"

joinWithDecimalSep :: Text -> (Text, Text) -> Text
joinWithDecimalSep sep (wholes, decimals) = wholes <> sep <> decimals
