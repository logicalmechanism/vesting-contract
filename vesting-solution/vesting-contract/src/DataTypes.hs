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
  ( compareVestingData
  , retainOwnership
  , updateRewardParams
  , VestingData
  , cdtVestingStage
  , cdtVestingUserPkh
  , cdtVestingUserSc
  , cdtStartingAmount
  , cdtDeltaAmount
  , cdtStartPoint
  , cdtLockPeriod
  , cdtTimeUnit
  , AddressData
  , aPkh
  , aSc
  , IncreaseData
  , iAmt
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the value object data object.
-------------------------------------------------------------------------------
data IncreaseData = IncreaseData
  { iAmt :: Integer
  -- ^ The amount to increase a finished vesting solution.
  }
PlutusTx.unstableMakeIsData ''IncreaseData
-------------------------------------------------------------------------------
-- | Create the address data object.
-------------------------------------------------------------------------------
data AddressData = AddressData
  { aPkh :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the user.
  , aSc  :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the user.
  }
PlutusTx.unstableMakeIsData ''AddressData
-------------------------------------------------------------------------------
-- | Create the vesting data object.
-------------------------------------------------------------------------------
data VestingData = VestingData
  { cdtVestingStage   :: Integer
  -- ^ The vesting stage determines the end time and reward.
  , cdtVestingUserPkh :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the vestor.
  , cdtVestingUserSc  :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the vestor.
  , cdtStartingAmount :: Integer
  -- ^ The starting reward amount at vesting stage 0.
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

-------------------------------------------------------------------------------
-- | Compare two vesting data objects. a is input b is output.
-------------------------------------------------------------------------------
compareVestingData :: VestingData -> VestingData -> Bool
compareVestingData a b =  ( cdtVestingStage   a + 1 == cdtVestingStage   b ) &&
                          ( cdtVestingUserPkh     a == cdtVestingUserPkh b ) &&
                          ( cdtVestingUserSc      a == cdtVestingUserSc  b ) &&
                          ( cdtStartingAmount     a == cdtStartingAmount b ) &&
                          ( cdtDeltaAmount        a == cdtDeltaAmount    b ) &&
                          ( cdtLockPeriod         a == cdtLockPeriod     b ) &&
                          ( cdtTimeUnit           a == cdtTimeUnit       b ) &&
                          ( (cdtStartPoint a) + (cdtLockPeriod a) == cdtStartPoint b )

retainOwnership :: VestingData -> VestingData -> Bool
retainOwnership a b = ( cdtVestingUserPkh a == cdtVestingUserPkh b ) &&
                      ( cdtVestingUserSc  a == cdtVestingUserSc  b )

updateRewardParams :: VestingData -> VestingData -> Bool
updateRewardParams a b =  ( cdtVestingStage   a == cdtVestingStage   b ) &&
                          ( cdtVestingUserPkh a == cdtVestingUserPkh b ) &&
                          ( cdtVestingUserSc  a == cdtVestingUserSc  b ) &&
                          ( cdtLockPeriod     a == cdtLockPeriod     b ) && -- updatable?
                          ( cdtTimeUnit       a == cdtTimeUnit       b )    -- updatable?