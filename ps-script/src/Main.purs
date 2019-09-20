module Main where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.TemplateString ((<->), (<^>))
import Data.Tuple.Nested ((/\))
import Database.PostgreSQL.PG (defaultPoolConfiguration, PGError, command, execute, newPool, Pool, Connection, query, Query(Query))
import Database.PostgreSQL.PG as PG
import Database.PostgreSQL.Row (Row0(Row0), Row3(Row3))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error, errorShow, log, logShow) as Console
import Effect.Console (log)

-- Amount of money in USD cents
type Money
  = Int

-- | DateTime in ISO 8601 string
type DateTime
  = String

type Campaign
  = { id :: String
    , name :: String
    , minProfitIncrease :: Money
    , startDate :: DateTime
    , endDate :: DateTime
    }

type Test
  = { sku :: String
    , originalVariantId :: String
    , testVariantId :: String
    , originalVariantPrice :: Money
    , testVariantPrice :: Money
    , productName :: String
    }

type ProductName
  = String

type Sku
  = String

experimentValues :: Array Test -> String
experimentValues = String.joinWith ",\n" <<< map testToRow
  where
  testToRow { sku, productName } = "('${sku}', '${productName}')" <^> [ "sku" /\ sku, "productName" /\ productName ]

insertExperimentSql :: Array Test -> String
insertExperimentSql tests =
  """
  INSERT INTO experiments
    (sku, product_name)
  VALUES
    ${values}
  ;
  """
    <-> [ "values" /\ (experimentValues tests) ]

buildSql :: Campaign -> Array Test -> String
buildSql campaign tests = ""

type PG a
  = ExceptT PGError Aff a

withConnection :: forall a. Pool -> (Connection -> PG a) -> PG a
withConnection = PG.withConnection runExceptT

main :: Effect Unit
main = launchAff_ $ runExceptT $ do
    pool <- liftEffect $ newPool ((defaultPoolConfiguration "purspg") { idleTimeoutMillis = Just 1000 })
    withConnection pool \conn -> do
      time <- query conn ( Query """ SELECT NOW(); """ :: Query String) Row0
      liftEffect $ Console.logShow time
  -- liftEffect $ case eRows of
  --                   Left err -> Console.errorShow err
  --                   Right rows -> Console.logShow rows
