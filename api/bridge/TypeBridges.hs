{-# LANGUAGE OverloadedStrings #-}

module TypeBridges where

import Control.Applicative
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes

suppleBridge :: BridgePart
suppleBridge = defaultBridge
  <|> priceBridge
  <|> skuBridge
  <|> expIdBridge
  <|> bucketIdBridge
  <|> svidBridge
  <|> pidBridge
  <|> imageBridge

priceBridge :: BridgePart
priceBridge = do
  typeName ^== "Price"
  return psNumber

skuBridge :: BridgePart
skuBridge = do
  typeName ^== "Sku"
  return psString

expIdBridge :: BridgePart
expIdBridge = do
  typeName ^== "ExpId"
  return psNumber

bucketIdBridge :: BridgePart
bucketIdBridge = do
  typeName ^== "BucketId"
  return psNumber

svidBridge :: BridgePart
svidBridge = do
  typeName ^== "Svid"
  return psNumber

pidBridge :: BridgePart
pidBridge = do
  typeName ^== "Pid"
  return psNumber

imageBridge :: BridgePart
imageBridge = do
  typeName ^== "Image"
  return psString
