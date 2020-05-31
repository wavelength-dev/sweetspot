module SweetSpot.ShopifyHelper where

import Data.Date (Weekday(..))
import Data.JSDate (JSDate)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Prelude (Unit)
import React.Basic.DOM (text) as R
import React.Basic.Hooks (JSX, element)
import SweetSpot.Shopify (datePicker, formLayout, heading, textContainer) as Shopify

data ElementTag
  = H2

elementTagToString :: ElementTag -> String
elementTagToString H2 = "h2"

heading :: ElementTag -> String -> JSX
heading elementTag text =
  element Shopify.heading
    { element: (elementTagToString elementTag)
    , children: R.text text
    }

textContainer :: Array JSX -> JSX
textContainer children = element Shopify.textContainer { children }

formLayout :: Array JSX -> JSX
formLayout children = element Shopify.formLayout { children }

weekdayToEnum :: Weekday -> Int
weekdayToEnum = case _ of
  Monday -> 1
  Tuesday -> 2
  Wednesday -> 3
  Thursday -> 4
  Friday -> 5
  Saturday -> 6
  Sunday -> 0

type DatePickerProps
  = { month :: Int
    , onChange :: EffectFn1 { end :: JSDate, start :: JSDate } Unit
    , onMonthChange :: EffectFn2 Int Int Unit
    , selected :: { end :: JSDate, start :: JSDate }
    , weekStartsOn :: Weekday
    , year :: Int
    }

datePicker :: DatePickerProps -> JSX
datePicker props =
  element Shopify.datePicker
    { weekStartsOn: weekdayToEnum props.weekStartsOn
    , month: props.month
    , year: props.year
    , onChange: props.onChange
    , onMonthChange: props.onMonthChange
    , selected: props.selected
    }
