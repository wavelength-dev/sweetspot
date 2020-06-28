module SweetSpot.Date where

import Data.DateTime (DateTime)
import Data.Foldable (fold)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime (format) as Formatter
import Data.Formatter.Number (Formatter(..))
import Data.List (List(..), (:))

formatDate :: DateTime -> String
formatDate dateTime =
  fold
    [ Formatter.format (MonthShort : Nil) dateTime
    , " "
    , Formatter.format (DayOfMonth : Nil) dateTime
    ]
