module Api.Request where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Data.Either (Either(..))

data Endpoint = Experiments

mkRequest :: forall m. MonadAff m => Endpoint -> m (Maybe Json)
mkRequest endpoint = do
  let path = case endpoint of
        Experiments -> "/api/experiments"

  res <- liftAff $ AX.get ResponseFormat.json path

  case res.body of
    Left err -> do
      log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
      pure Nothing
    Right json -> pure $ Just json
