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
module HelperFunctions
  ( isAddrGettingPaid
  , isNInputs
  , isNOutputs
  , createAddress
  , createBuiltinByteString
  , isTxOutsideInterval
  , rewardFunction
  , calculateEndTime
  , totalReward
  , pow
  , findFactors
  ) where
import           PlutusTx.Prelude 
import           Plutus.V1.Ledger.Credential
import qualified Plutus.V1.Ledger.Time       as Time
import qualified Plutus.V1.Ledger.Interval   as Interval
import qualified Plutus.V1.Ledger.Value      as Value
import qualified Plutus.V2.Ledger.Api        as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | factors of t
-------------------------------------------------------------------------------
findFactors :: Integer -> (Integer, Integer)
findFactors number = ( count shift 2 0, count shift 5 0 )
  where
    shift = number + 1

    count n b counter = 
      if modulo n b == 0
        then count (divide n b) b (counter + 1)
        else counter
-------------------------------------------------------------------------------
-- | optimal power function
-------------------------------------------------------------------------------
pow:: Integer -> Integer -> Integer
pow x n = if n == 0 then 1 else if n == 1 then x else
  if even n
    then pow x ( divide n 2 ) * pow x ( divide n 2 )
    else  x * pow x ( n - 1 )
-------------------------------------------------------------------------------
-- | Total reward for f = v - t*d
-------------------------------------------------------------------------------
totalReward :: Integer -> Integer -> Integer
totalReward v0 deltaV = divide (v0 * (v0 + deltaV)) (2 * deltaV)
-------------------------------------------------------------------------------
-- | Assume Linear reward f = v - t*d
-------------------------------------------------------------------------------
rewardFunction :: Integer -> Integer -> Integer -> Integer
rewardFunction v0 deltaV t = if value >= 0 then value else 0
  where
    value :: Integer
    value = v0 - t * deltaV
-------------------------------------------------------------------------------
-- | Calculates the ending time in unix time for some vestment.
-------------------------------------------------------------------------------
calculateEndTime :: Integer -> Integer -> Integer
calculateEndTime startDay lockedPeriod = endingTime
  where
  -- unix time at epoch 312
    timeTilRefEpoch :: Integer
    -- timeTilRefEpoch = 1640987100000  -- mainnet
    timeTilRefEpoch = 1640895900000  -- testnet

    -- time unit
    lengthOfDay :: Integer
    lengthOfDay = 86400000

    -- starting Time is just the reference plus how many days in nanoseconds.
    startingTime :: Integer
    startingTime = timeTilRefEpoch + startDay * lengthOfDay

    -- ending time is just starting time plus the vesting period.
    endingTime :: Integer
    endingTime = startingTime + lockedPeriod * lengthOfDay
-------------------------------------------------------------------------------
-- | Pick the locking interval, assume negative inf to endingTime.
-------------------------------------------------------------------------------
lockInterval :: Integer -> PlutusV2.Interval PlutusV2.POSIXTime
lockInterval endingTime = Interval.to (integerToPOSIX endingTime)
  where
    -- Number of milliseconds from unix time start
    integerToPOSIX :: Integer -> PlutusV2.POSIXTime
    integerToPOSIX x = Time.fromMilliSeconds $ Time.DiffMilliSeconds x
-------------------------------------------------------------------------
-- | Check if outside the interval for the tx
-------------------------------------------------------------------------
isTxOutsideInterval :: Integer -> PlutusV2.POSIXTimeRange -> Bool
isTxOutsideInterval endingTime txValidityRange = not $ Interval.overlaps timeRange txValidityRange
  where
    timeRange :: PlutusV2.Interval PlutusV2.POSIXTime
    timeRange = lockInterval endingTime
-------------------------------------------------------------------------
-- | Appends two bytestrings together from a list, element by element
-------------------------------------------------------------------------
flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
flattenBuiltinByteString [] = emptyByteString 
flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)
-------------------------------------------------------------------------
-- | Creates a proper BuiltinByteString.
-------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString | x <- intList]
-------------------------------------------------------------------------
-- | Create a proper Address Type.
-------------------------------------------------------------------------
createAddress :: PlutusV2.PubKeyHash -> PlutusV2.PubKeyHash -> PlutusV2.Address
createAddress pkh sc = 
  if PlutusV2.getPubKeyHash sc == emptyByteString 
    then PlutusV2.Address (PubKeyCredential pkh) Nothing 
    else PlutusV2.Address (PubKeyCredential pkh) (Just $ StakingHash $ PubKeyCredential sc)
-------------------------------------------------------------------------------
-- | Search each TxOut for an addr and value.
-------------------------------------------------------------------------------
isAddrGettingPaid :: [PlutusV2.TxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
isAddrGettingPaid []     _    _ = False
isAddrGettingPaid (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = isAddrGettingPaid xs addr val
  where
    checkAddr :: Bool
    checkAddr = PlutusV2.txOutAddress x == addr

    checkVal :: Bool
    checkVal = Value.geq (PlutusV2.txOutValue x) val
-------------------------------------------------------------------------------
-- | Force a number of inputs to have datums
-------------------------------------------------------------------------------
isNInputs :: [PlutusV2.TxInInfo] -> Integer -> Bool
isNInputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [PlutusV2.TxInInfo] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved x of
        PlutusV2.NoOutputDatum       -> loopInputs xs counter
        (PlutusV2.OutputDatumHash _) -> loopInputs xs (counter + 1)
        (PlutusV2.OutputDatum     _) -> loopInputs xs (counter + 1)
-------------------------------------------------------------------------------
-- | Force a number of outputs to have datums
-------------------------------------------------------------------------------
isNOutputs :: [PlutusV2.TxOut] -> Integer -> Bool
isNOutputs utxos number = loopInputs utxos 0
  where
    loopInputs :: [PlutusV2.TxOut] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> loopInputs xs counter
        (PlutusV2.OutputDatumHash _) -> loopInputs xs (counter + 1)
        (PlutusV2.OutputDatum     _) -> loopInputs xs (counter + 1)