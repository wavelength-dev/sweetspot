{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SweetSpot.Database.Migrations.V0003AddUserCartTokens
  ( module SweetSpot.Database.Migrations.V0002AddSessions
  , module SweetSpot.Database.Migrations.V0003AddUserCartTokens
  ) where

import Database.Beam
import Database.Beam.Postgres (Postgres, PgExtensionEntity)
import Database.Beam.Postgres.Syntax (pgTextType)
import Database.Beam.Postgres.PgCrypto (PgCrypto)
import Database.Beam.Migrate

import SweetSpot.Database.Migrations.V0002AddSessions hiding
  ( SweetSpotDb(..)
  , migration
  , productVariants
  , campaigns
  , checkoutEvents
  , checkoutItems
  , cryptoExtension
  , events
  , installNonces
  , shops
  , treatments
  , userExperiments
  , users
  , sessions
  )
import qualified SweetSpot.Database.Migrations.V0002AddSessions as V2
import SweetSpot.Data.Common

-- | ---------------------------------------------------------------------------
-- | UserCartToken
-- | ---------------------------------------------------------------------------
data UserCartTokenT f
  = UserCartToken
  { _cartTokenId :: Columnar f CartToken
  , _cartTokenUser :: PrimaryKey V2.UserT f
  } deriving (Generic, Beamable)

type UserCartToken =  UserCartTokenT Identity
type UserCartTokenKey = PrimaryKey UserCartTokenT Identity

instance Table UserCartTokenT where
  data PrimaryKey UserCartTokenT f
    = UserCartTokenKey (Columnar f CartToken) deriving (Generic, Beamable)
  primaryKey = UserCartTokenKey . _cartTokenId

UserCartToken (LensFor cartTokenId) (V2.UserKey (LensFor cartTokenUser)) = tableLenses

cartTokenType :: DataType Postgres CartToken
cartTokenType = DataType pgTextType

-- | ---------------------------------------------------------------------------
-- | Database
-- | ---------------------------------------------------------------------------
data SweetSpotDb f = SweetSpotDb
  { _shops :: f (TableEntity V2.ShopT)
  , _installNonces :: f (TableEntity V2.InstallNonceT)
  , _users :: f (TableEntity V2.UserT)
  , _campaigns :: f (TableEntity V2.CampaignT)
  , _productVariants :: f (TableEntity V2.ProductVariantT)
  , _treatments :: f (TableEntity V2.TreatmentT)
  , _userExperiments :: f (TableEntity V2.UserExperimentT)
  , _checkoutEvents :: f (TableEntity V2.CheckoutEventT)
  , _checkoutItems :: f (TableEntity V2.CheckoutItemT)
  , _events :: f (TableEntity V2.EventT)
  , _cryptoExtension :: f (PgExtensionEntity PgCrypto)
  , _sessions :: f (TableEntity V2.SessionT)
  , _userCartTokens :: f (TableEntity UserCartTokenT)
  } deriving (Generic)

instance Database Postgres SweetSpotDb

SweetSpotDb (TableLens shops) (TableLens installNonces) (TableLens users) (TableLens campaigns) (TableLens productVariants) (TableLens treatments) (TableLens userExperiments) (TableLens checkoutEvents) (TableLens checkoutItems) (TableLens events) (TableLens cryptoExtension) (TableLens sessions) (TableLens userCartTokens) = dbLenses

-- | ---------------------------------------------------------------------------
-- | Migration
-- | ---------------------------------------------------------------------------
migration
  :: CheckedDatabaseSettings Postgres V2.SweetSpotDb
  -> Migration Postgres (CheckedDatabaseSettings Postgres SweetSpotDb)
migration currentDb = SweetSpotDb
  <$> preserve (V2._shops currentDb)
  <*> preserve (V2._installNonces currentDb)
  <*> preserve (V2._users currentDb)
  <*> preserve (V2._campaigns currentDb)
  <*> preserve (V2._productVariants currentDb)
  <*> preserve (V2._treatments currentDb)
  <*> preserve (V2._userExperiments currentDb)
  <*> preserve (V2._checkoutEvents currentDb)
  <*> preserve (V2._checkoutItems currentDb)
  <*> preserve (V2._events currentDb)
  <*> preserve (V2._cryptoExtension currentDb)
  <*> preserve (V2._sessions currentDb)
  <*> createTable "user_cart_tokens"
    UserCartToken { _cartTokenId = field "cart_token" cartTokenType notNull
                  , _cartTokenUser = V2.UserKey $ field "user_id" V2.userIdType notNull
                  }
