module SweetSpot.Service where

import SweetSpot.Data.Api
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Argonaut (Json, jsonEmptyObject, jsonParser, (:=), (~>))
import Data.Argonaut (stringify) as Argonaut
import Data.Array as Array
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, error)
import Effect.Aff (attempt) as Aff
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow) as Unsafe
import Milkis (Options, Response, Fetch)
import Milkis as Milkis
import Milkis.Impl.Window as MilkisImpl
import Prelude (class Show, Unit, bind, discard, map, pure, show, unit, unless, (#), ($), (<>), (==), (>>=), (>>>))
import Record.Unsafe.Union as RecordUnsafe
import SweetSpot.CampaignListPage (CampaignId)
import SweetSpot.Data.Codec as Codec
import SweetSpot.Env (apiUrl) as Env
import SweetSpot.Logger (LogLevel(..)) as LogLevel
import SweetSpot.Logger as Logger
import SweetSpot.QueryString (buildQueryString) as QueryString
import SweetSpot.Session (SessionId(..))

fetch :: Fetch
fetch = Milkis.fetch MilkisImpl.windowFetch

getJson :: forall options. String -> Record options -> Aff Response
getJson url options = fetch (Milkis.URL url) combinedOptions
  where
  defaults =
    { method: Milkis.getMethod
    , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
    }

  combinedOptions :: Record Options
  combinedOptions = RecordUnsafe.unsafeUnion options defaults

serviceUrl :: String
serviceUrl = Env.apiUrl <> "/api/dashboard/"

data Resource
  = Campaigns
  | Products

instance showResource :: Show Resource where
  show Campaigns = "campaigns"
  show Products = "products"

getResourceRoute :: Resource -> String
getResourceRoute Campaigns = serviceUrl <> "campaigns"

getResourceRoute Products = serviceUrl <> "products"

fetchResource :: Resource -> SessionId -> Maybe String -> Aff Json
fetchResource resource (SessionId sessionId) mPageInfo = do
  res <- getJson (route <> queryString) {}
  let
    status = Milkis.statusCode res
  body <- Milkis.text res
  unless (status == 200) do
    Logger.logWithContext LogLevel.Error "failed to fetch resource" { status: show status, body } # liftEffect
    "failed to fetch resource " <> show resource # error >>> throwError
  case jsonParser body of
    Left parseErrorMessage -> throwError $ error $ parseErrorMessage
    Right json -> pure json
  where
  route = getResourceRoute resource

  sessionParam = Tuple "session" sessionId # Just

  pageParam = case mPageInfo of
    Nothing -> Nothing
    Just pageInfo -> Tuple "page_info" pageInfo # Just

  queryParams = Array.catMaybes [ sessionParam, pageParam ]

  queryString =
    QueryString.buildQueryString queryParams
      # case _ of
          Left errMsg -> Unsafe.unsafeThrow errMsg
          Right qs -> qs

decodeOrThrow :: forall m b. MonadThrow Error m => Either String b -> m b
decodeOrThrow = case _ of
  Left decodeErrorMessage -> throwError $ error decodeErrorMessage
  Right result -> pure result

fetchCampaigns :: SessionId -> Aff (Array UICampaign)
fetchCampaigns sessionId =
  fetchResource Campaigns sessionId Nothing
    >>= Codec.decodeUICampaigns
    >>> decodeOrThrow

fetchProducts :: SessionId -> Maybe String -> Aff ProductsResponse
fetchProducts sessionId pageInfo =
  fetchResource Products sessionId pageInfo
    >>= Codec.decodeProductsResponse
    >>> decodeOrThrow

encodeCreateVariant :: CreateVariant -> Json
encodeCreateVariant createVariant =
  let
    svid = view createVariantSvid createVariant

    price = view createVariantPrice createVariant
  in
    "_createVariantSvid" := svid
      ~> "_createVariantPrice"
      := price
      ~> jsonEmptyObject

encodeCreateExperiment :: CreateExperiment -> Json
encodeCreateExperiment createExperiment =
  let
    productId = view createExperimentProductId createExperiment

    variants = view createExperimentVariants createExperiment
  in
    "_createExperimentProductId" := productId
      ~> "_createExperimentVariants"
      := map encodeCreateVariant variants
      ~> jsonEmptyObject

encodeCreateCampaign :: CreateCampaign -> Json
encodeCreateCampaign createCampaign =
  "_createCampaignName" := (view createCampaignName createCampaign)
    ~> "_createCampaignExperiments"
    := map encodeCreateExperiment (view createCampaignExperiments createCampaign)
    ~> jsonEmptyObject

makeCampaign :: SessionId -> CreateCampaign -> Aff (Either String Unit)
makeCampaign (SessionId id) createCampaign =
  let
    options =
      { method: Milkis.postMethod
      , body: Argonaut.stringify $ encodeCreateCampaign createCampaign
      , headers: Milkis.makeHeaders { "Content-Type": "application/json" }
      }
  in
    Aff.attempt
      (fetch (Milkis.URL (serviceUrl <> "campaigns?session=" <> id)) options)
      >>= case _ of
          Left requestErrMsg -> requestErrMsg # show >>> Left >>> pure
          Right res -> pure $ pure unit

stopCampaign :: SessionId -> CampaignId -> Aff Unit
stopCampaign (SessionId sessionId) campaignId = do
  let
    options = { method: Milkis.postMethod }

    url =
      Milkis.URL
        ( serviceUrl
            <> "campaigns/"
            <> campaignId
            <> "/stop"
            <> "?session="
            <> sessionId
        )
  res <- fetch url options
  let
    status = Milkis.statusCode res
  body <- Milkis.text res
  unless (status == 200) do
    liftEffect $ Logger.logWithContext LogLevel.Error "failed to stop experiment" { body }
