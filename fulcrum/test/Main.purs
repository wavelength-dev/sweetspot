module Test.Main where

import Prelude

import Effect (Effect)
import Test.RuntimeDependency (tests) as RuntimeDependency
import Test.Unit.Main (runTest)
import Test.QueryString (tests) as QueryString
import Test.User (tests) as User

main :: Effect Unit
main = runTest do
  RuntimeDependency.tests
  User.tests
  QueryString.tests
