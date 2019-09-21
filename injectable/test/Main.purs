module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Maybe (fromJust) as Maybe
import Effect (Effect)
import Partial.Unsafe (unsafePartial) as Unsafe
import SweetSpot.Data.Event (Page(..))
import SweetSpot.Event.PageDetection (pageFromPathname) as PageDetection
import Test.Unit (test)
import Test.Unit.Assert (equal) as Assert
import Test.Unit.Main (runTest)

getSampleUrl :: Page -> String
getSampleUrl = case _ of
  Cart -> "/cart"
  Checkout -> "/27218906/checkouts/491b70643fad3202820a925f0719bea6?_ga=2.64881327.1460844967.1568818574-1594420487.1568467322&discount="
  Collection -> "/collections/mens-caiman-series"
  Collections -> "/collections/"
  Home -> "/"
  Product -> "/collections/all/products/mens-navy-blue-w-silver-details"
  Unknown -> "/pages/our-story"

detectFromSample :: Page -> Page
detectFromSample = Unsafe.unsafePartial $ getSampleUrl >>> PageDetection.pageFromPathname >>> Maybe.fromJust

main :: Effect Unit
main =
  runTest do
    test "recognizes the cart page" do
      Assert.equal Cart $ detectFromSample Cart
    test "recognizes the product page" do
      Assert.equal Product $ detectFromSample Product
      Assert.equal (Just Product) (PageDetection.pageFromPathname "/collections/all/products/mens-navy-blue-w-silver-details?variant=14386685214763")
    test "recognizes the listings page" do
      Assert.equal Collection $ detectFromSample Collection
    test "recognizes the collections page" do
      Assert.equal Collections $ detectFromSample Collections
    test "recognizes the home page" do
      Assert.equal Home $ detectFromSample Home
    test "recognizes the checkout page" do
      Assert.equal Checkout $ detectFromSample Checkout
