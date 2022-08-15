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
  ( VestingData
  , cdtVestingStage
  , cdtVestingUserPkh
  , cdtVestingUserSc
  , cdtStartingAmount
  , cdtDeltaAmount
  , cdtStartPoint
  , cdtLockPeriod
  , cdtTimeUnit
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the vesting data object.
-------------------------------------------------------------------------------
data VestingData = VestingData
  { cdtVestingStage   :: Integer
  -- ^ The vesting stage determines the deadline and reward.
  , cdtVestingUserPkh :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVestingUserSc  :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the receiver.
  , cdtStartingAmount :: Integer
  -- ^ The starting reward amount at stage 0.
  , cdtDeltaAmount    :: Integer
  -- ^ The decrease to the reward amount per vesting stage.
  , cdtStartPoint     :: Integer
  -- ^ The starting point is the number of time units from the reference time
  , cdtLockPeriod     :: Integer
  -- ^ The lock period is the number of time units between vestments.
  , cdtTimeUnit       :: Integer
  -- ^ The time unit used for counting.
  }
PlutusTx.unstableMakeIsData ''VestingData

-- old is a; new is b
instance Eq VestingData where
  {-# INLINABLE (==) #-}
  a == b = ( cdtVestingStage   a + 1 == cdtVestingStage   b ) &&
           ( cdtVestingUserPkh     a == cdtVestingUserPkh b ) &&
           ( cdtVestingUserSc      a == cdtVestingUserSc  b ) &&
           ( cdtStartingAmount     a == cdtStartingAmount b ) &&
           ( cdtDeltaAmount        a == cdtDeltaAmount    b ) &&
           ( cdtLockPeriod         a == cdtLockPeriod     b ) &&
           ( cdtTimeUnit           a == cdtTimeUnit       b ) &&
           ( (cdtStartPoint a) + (cdtLockPeriod a) == cdtStartPoint b )