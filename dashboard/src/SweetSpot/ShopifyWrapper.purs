module SweetSpot.ShopifyWrapper where

import React.Basic (JSX, element)
import React.Basic (fragment) as React
import React.Basic.DOM (text) as R
import SweetSpot.Shopify (heading, textContainer) as Shopify

data Element
  = H2

elementToHeading :: Element -> String
elementToHeading = case _ of
  H2 -> "h2"

heading :: { element :: Element, text :: String } -> JSX
heading props =
  element Shopify.heading
    { element: elementToHeading props.element, children: R.text props.text
    }

textContainer :: Array JSX -> JSX
textContainer children = element Shopify.textContainer { children: React.fragment children }
