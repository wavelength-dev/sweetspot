module Supple.Component.Polaris where

import Halogen.HTML as HH
import Supple.Component.Util (css)

layout :: forall i p. Array (HH.HTML i p) -> HH.HTML i p
layout cs =
  HH.div
    [ css "Polaris-Layout" ]
    cs

section :: forall i p. Array (HH.HTML i p) -> HH.HTML i p
section cs =
  HH.div
    [ css "Polaris-Layout__Section"]
    cs


card :: forall i p. String -> Array (HH.HTML i p) -> HH.HTML i p
card title cs =
  HH.div
    [ css "Polaris-Card" ]
    [ HH.div
      [ css "Polaris-Card__Header" ]
      [ HH.h2
        [ css "Polaris-Heading" ]
        [ HH.text title ]
      ]

    , HH.div
        [ css "Polaris-Card__Section" ]
        cs
    ]
