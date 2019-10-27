module Test.QueryString where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Sprice.QueryString (QueryParam(..), parseQueryString)
import Test.Unit (TestF, TestSuite, suite, test)
import Test.Unit.Assert (equal) as Assert

tests :: TestSuite
tests =
  suite "querystring parsing" do
    test "parses simple query string" do
      Assert.equal [ Right $ QueryParam "name" (Just "alex") ] $ parseQueryString "name=alex"
    test "parses multiple querystring parameters" do
      Assert.equal [ Right $ QueryParam "name" (Just "alex"), Right $ QueryParam "age" (Just "26") ] $ parseQueryString "name=alex&age=26"
    test "parses querystring with questionmark" do
      Assert.equal [ Right $ QueryParam "name" (Just "alex") ] $ parseQueryString "?name=alex"
    test "parses empty value querystring" do
      Assert.equal [ Right $ QueryParam "name" Nothing ] $ parseQueryString "name="
    test "parsing key-less querystring results in error" do
      Assert.equal [ Left "Parameter without a key" ] $ parseQueryString "=alex"
    test "parsing empty querystring results in error" do
      Assert.equal [ Left "Parameter without an '='" ] $ parseQueryString ""
    test "parsing value-ambiguous querystring results in error" do
      Assert.equal [ Left "More than one '=' in single parameter" ] $ parseQueryString "name=alex=niclas"
    test "parsing value-less querystring results in error" do
      Assert.equal [ Left "Parameter without an '='" ] $ parseQueryString "name"
