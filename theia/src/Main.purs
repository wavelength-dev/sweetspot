module Main where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..), either)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Foreign (readArray, readNumber, readString)
import Test.Unit (failure, test, testSkip)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTestWith)
import Test.Unit.Output.Fancy as Fancy
import Toppokki (Page, Selector)
import Toppokki as T

atLeastOneMatchingElement :: Selector -> Page -> Aff Unit
atLeastOneMatchingElement selector page = do
  lengthF <-
    T.unsafePageEvalAll
      selector
      "els => els.length"
      page
  case runExcept $ readNumber lengthF of
    Left _ -> failure "failed to find any elements"
    Right length -> Assert.assert "number of elements is not larger than zero" (length > 0.0)

data LongvadonPage
  = MensBlack
  | WatchListings

getUrl :: LongvadonPage -> T.URL
getUrl MensBlack = T.URL "https://longvadon.com/collections/mens-caiman-series/products/mens-midnight-black-w-black-details"
getUrl WatchListings = T.URL "https://longvadon.com/collections/mens-caiman-series/"

main :: Effect Unit
main =
  runAff_ (either throwError pure) do
    -- beforeAll
    browser <- T.launch {}
    page <- T.newPage browser

    runTestWith Fancy.runTest do

      test "finds tagged price elements on a product page" do
         T.goto (getUrl MensBlack) page
         -- T.pageWaitForSelector (T.Selector "[class*=sweetspot__price_id]") {} page
         atLeastOneMatchingElement (T.Selector "[class*=sweetspot__price_id]") page

      test "finds tagged price elements on a collections page" do
         T.goto (getUrl WatchListings) page
         -- T.pageWaitForSelector (T.Selector "[class*=sweetspot__price_id]") {} page
         atLeastOneMatchingElement (T.Selector "[class*=sweetspot__price_id]") page

      test "tags the product page price element" do
         T.goto (getUrl MensBlack) page
         -- T.pageWaitForSelector (T.Selector "[class*=sweetspot__price_id]") {} page
         atLeastOneMatchingElement
           (T.Selector ".money.sweetspot__price_id--LVMens1Black42ClaspB")
           page

      testSkip "swaps watch variant ids so correct item gets checked out" do
         -- TODO: swap with price variant ids
         let expectedVariantIds = ["1", "2", "3", "4"]
         T.goto (getUrl MensBlack) page
         -- _ <- T.pageWaitForSelector (T.Selector "[class*=sweetspot__price_id]") {} page
         variantIdsF <-
           T.unsafePageEvalAll
             (T.Selector "option[data-sku^=LVMens1Black]")
             "els => els.map(el => el.getAttribute('value'))"
             page
         case runExcept $ readArray variantIdsF >>= traverse readString of
              Left _ -> failure "failed to retrieve a list of variantIds"
              Right variantIds -> Assert.equal expectedVariantIds variantIds

    -- afterAff
    T.close browser
