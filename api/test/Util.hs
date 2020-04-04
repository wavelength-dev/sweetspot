module Util where

import qualified Control.Concurrent as C
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust)
import Data.UUID (fromText)
import RIO
import SweetSpot.Data.Common
import SweetSpot.Server (runServer)
import System.Environment (setEnv)

magicWaitNumber = 3 * 1000 * 1000

toUUID = fromJust . fromText

shopDomain = ShopDomain "test-shop.myshopify.com"

invalidDomain = ShopDomain "lol-shop.myshopify.com"

newShopDomain = ShopDomain "new-shop.myshopify.com"

user1 = UserId $ toUUID "2eb6a046-6609-4518-ab23-87f1ad56bbaa"

user2 = UserId $ toUUID "e3b937e7-ac65-4324-9d67-040cdc35b555"

user3 = UserId $ toUUID "85271f15-683b-4972-bd68-b7aaacdeb70d"

unknownUser = UserId $ toUUID "8a2492c7-82f8-4845-844a-00589d270f66"

campaign1 = CampaignId $ toUUID "6072b6ea-7c37-4b26-80cd-f8f87d05a991"

campaign2 = CampaignId $ toUUID "6072b6ea-7c37-4b26-80cd-f8f87d05a992"

campaign3 = CampaignId $ toUUID "6072b6ea-7c37-4b26-80cd-f8f87d05a993"

unknownCampaign = CampaignId $ toUUID "fec505ce-4100-4c3f-a55b-608b14688c52"

withApi :: IO ()
withApi = do
  runInThread
  C.threadDelay magicWaitNumber
  where
    runInThread = liftIO $ C.forkIO runServer

withEnv :: [(String, String)] -> IO ()
withEnv = traverse_ $ uncurry setEnv
