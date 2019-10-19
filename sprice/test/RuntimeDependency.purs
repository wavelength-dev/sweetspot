module Test.RuntimeDependency where

import Prelude

import Control.Monad.Free (Free)
import Effect.Class (liftEffect)
import Sprice.RuntimeDependency (getIsRuntimeAdequate)
import Test.Unit (TestF, test)
import Test.Unit.Assert (assertFalse) as Assert

tests :: Free TestF Unit
tests = test "finds runtime inadequate" do
  -- fetch will be missing on Node which is what the tests run on
  isRuntimeAdequate <- liftEffect getIsRuntimeAdequate
  Assert.assertFalse "runtime should be inadequate" isRuntimeAdequate
