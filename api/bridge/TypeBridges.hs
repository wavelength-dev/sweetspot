module TypeBridges where

import Control.Applicative
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes
import Language.PureScript.Bridge.TypeInfo (TypeInfo (..))
import RIO

psDate =
  TypeInfo
    { _typePackage = "purescript-datetime",
      _typeModule = "Data.DateTime",
      _typeName = "DateTime",
      _typeParameters = []
    }

sweetspotBridge :: BridgePart
sweetspotBridge =
  defaultBridge
    <|> (typeName ^== "Price" >> return psNumber)
    <|> (typeName ^== "Sku" >> return psString)
    <|> (typeName ^== "Svid" >> return psString)
    <|> (typeName ^== "Pid" >> return psString)
    <|> (typeName ^== "CampaignId" >> return psString)
    <|> (typeName ^== "UTCTime" >> return psDate)
    <|> (typeName ^== "CartToken" >> return psString)
    <|> (typeName ^== "UserId" >> return psString)
