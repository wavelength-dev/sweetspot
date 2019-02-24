module Supple.Api.Request where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Supple.Data.Api (CreateExperiment)

data Endpoint
  = ExperimentsE
  | ProductsE
  | CreateExperimentE CreateExperiment

mkRequest :: forall m. MonadAff m => Endpoint -> m (Maybe Json)
mkRequest endpoint = do
  let path = case endpoint of
        ExperimentsE -> "/api/experiments"
        ProductsE -> "/api/products"
        CreateExperimentE _ -> "/api/experiments"


  res <- case endpoint of
    CreateExperimentE body -> liftAff $ AX.post ResponseFormat.json path (RequestBody.json $ encodeJson body)
    _ -> liftAff $ AX.get ResponseFormat.json path

  case res.body of
    Left err -> do
      log $ "GET /api response failed to decode: " <> AX.printResponseFormatError err
      pure Nothing
    Right json -> pure $ Just json
