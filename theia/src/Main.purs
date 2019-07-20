module Main where

import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Unit.Assert as Assert
import Toppokki as T

main :: Effect Unit
main = launchAff_ do
  browser <- T.launch {}
  page <- T.newPage browser
  T.goto (T.URL "https://libertyprice.myshopify.com/password") page
  T.click (T.Selector "a.js-modal-open-login-modal") page
  T.type_ (T.Selector "input#Password") "fropla" {} page
  T.press (T.KeyName "Enter") {} page
  T.goto (T.URL "https://libertyprice.myshopify.com/collections/summer/products/stripy-pants") page
  content <- T.content page
  Assert.assert "content is non-empty string" (String.length content > 0)
  _ <- T.screenshot {path: "./test/test.png"} page
  T.close browser
