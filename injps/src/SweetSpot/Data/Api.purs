module SweetSpot.Data.Api where

type UserBucket =
  { _ubUserId :: Number
  , _ubSku :: String
  , _ubOriginalSvid :: Number
  , _ubTestSvid :: Number
  , _ubPrice :: Number
  , _ubExpId :: Number
  , _ubBucketId :: Number
  , _ubBucketType :: String
  }
