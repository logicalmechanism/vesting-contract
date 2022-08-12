{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module DataTypes
  ( CustomDatumType
  , cdtVestingStage
  , cdtVestingUserPkh
  , cdtVestingUserSc
  , cdtStartingAmount
  , cdtDeltaAmount
  , cdtLockPeriod
  , cdtStartDay
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtVestingStage   :: Integer
  -- ^ The stage determines the deadline and reward.
  , cdtVestingUserPkh :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVestingUserSc  :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the receiver.
  , cdtStartingAmount :: Integer
  -- ^ The starting amount.
  , cdtDeltaAmount :: Integer
  -- ^ The delta amount to retrieve.
  , cdtLockPeriod :: Integer
  -- ^ The lock period in days between vestments.
  , cdtStartDay :: Integer
  -- ^ The starting day counted from epoch 312
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
-- old is a; new is b
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtVestingStage   a + 1 == cdtVestingStage   b ) &&
           ( cdtVestingUserPkh     a == cdtVestingUserPkh b ) &&
           ( cdtVestingUserSc      a == cdtVestingUserSc  b ) &&
           ( cdtStartingAmount     a == cdtStartingAmount b ) &&
           ( cdtDeltaAmount        a == cdtDeltaAmount    b ) &&
           ( cdtLockPeriod         a == cdtLockPeriod     b ) &&
           ( (cdtStartDay a) + (cdtLockPeriod a) == cdtStartDay b )