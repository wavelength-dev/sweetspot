module SweetSpot.Spacing where

import React.Basic.DOM (css, div) as R
import React.Basic.Hooks (JSX)

small :: JSX
small = R.div { style: R.css { height: "0.8rem", width: "0.8rem" } }

medium :: JSX
medium = R.div { style: R.css { height: "1.6rem", width: "1.6rem" } }

large :: JSX
large = R.div { style: R.css { height: "3.2rem", width: "3.2rem" } }
