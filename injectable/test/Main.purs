module Test.Main where

import Prelude
import Effect (Effect)
import Test.QueryString (queryStringTests)
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    queryStringTests
