module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.RuntimeDependency (tests) as RuntimeDependency

main :: Effect Unit
main = runTest do
  RuntimeDependency.tests
