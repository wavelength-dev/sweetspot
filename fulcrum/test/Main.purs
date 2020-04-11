module Test.Main where

import Prelude

import Effect (Effect)
import Test.RuntimeDependency (tests) as RuntimeDependency
import Test.Unit.Main (runTest)
import Test.QueryString (tests) as QueryString

main :: Effect Unit
main = runTest do
  RuntimeDependency.tests
  QueryString.tests
