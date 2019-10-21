module Test.User where

import Prelude

import Control.Monad.Writer (class MonadTell, Writer, runWriter)
import Control.Monad.Writer (tell) as Writer
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Sprice.User (class LocalStorageAction, UserId(..), getUserId, setUserId, uidStorageKey)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal) as Assert

newtype TestM a
  = TestM (Writer (Array (Tuple String String)) a)

derive newtype instance applicativeTestM :: Applicative TestM

derive newtype instance applyTestM :: Apply TestM

derive newtype instance bindTestM :: Bind TestM

derive newtype instance functorTestM :: Functor TestM

derive newtype instance monadTestM :: Monad TestM

derive newtype instance monadTellTestM :: MonadTell (Array (Tuple String String)) TestM

runMocked :: forall a. TestM a -> Tuple a (Array (Tuple String String))
runMocked (TestM w) = runWriter w

instance mockStorageLocalStorageAction :: LocalStorageAction TestM where
  getItem _ = "alex" # Just >>> pure
  setItem key value = Writer.tell $ [Tuple key value]

tests :: TestSuite
tests = suite "user tracking" do
  test "reads uid from local storage" do
    let (Tuple mUid _) = runMocked $ getUserId
    Assert.equal (Just $ UserId "alex") mUid
  test "writes userId to local storage" do
    let (Tuple _ writes) = runMocked $ setUserId (UserId "alex")
    Assert.equal (Just $ Tuple uidStorageKey "alex") (writes !! 0)
