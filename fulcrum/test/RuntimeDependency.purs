module Test.RuntimeDependency where

import Prelude

import Effect.Class (liftEffect)
import Sprice.RuntimeDependency (getIsRuntimeAdequate)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert (assertFalse) as Assert

tests :: TestSuite
tests = test "finds runtime inadequate" do
  -- fetch will be missing on Node which is what the tests run on
  isRuntimeAdequate <- liftEffect getIsRuntimeAdequate
  Assert.assertFalse "runtime should be inadequate" isRuntimeAdequate
