module SweetSpot.Util
  ( nanToZero,
    formatPrice,
  )
where

import Data.Scientific
import RIO
import qualified RIO.List as L
import qualified RIO.Text as T
import SweetSpot.Data.Common

nanToZero :: Double -> Double
nanToZero double = if isNaN double then 0 else double

data ThousandSep = Point Bool | Comma Bool | Apostrophe

formatPrice :: MoneyFormat -> Price -> FormattedPrice
formatPrice (MoneyFormat format) (Price price) =
  FormattedPrice (currencySymbol <> formattedPrice)
  where
    -- TODO: properly parse location of currency symbol
    currencySymbol = T.take 1 format
    formattedPrice = case T.drop 1 format of
      "{{amount}}" -> withFormat (Point True) price
      "{{amount_no_decimals}}" -> withFormat (Point False) price
      "{{amount_with_comma_separator}}" -> withFormat (Comma True) price
      "{{amount_no_decimals_with_comma_separator}}" -> withFormat (Comma False) price
      "{{amount_with_apostrophe_separator}}" -> withFormat Apostrophe price
      _ -> error "Unexpected money format"

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
