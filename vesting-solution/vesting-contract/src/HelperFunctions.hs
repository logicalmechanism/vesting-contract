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
  , isAddrHolding
  , isNInputs
  , isNOutputs
  , createAddress
  , createBuiltinByteString
  , isTxOutsideInterval
  , rewardFunction
  , calculateEndTime
  , totalReward
  , pow
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
-- | For Testing Only.
-------------------------------------------------------------------------------
pow:: Integer -> Integer -> Integer
pow x n = if n == 0 then 1 else if n == 1 then x else
  if even n
    then pow x ( divide n 2 ) * pow x ( divide n 2 )
    else  x * pow x ( n - 1 )

-------------------------------------------------------------------------------
-- | For Testing Only.
-------------------------------------------------------------------------------
totalReward :: Integer -> Integer -> Integer
totalReward v0' deltaV' = if deltaV' == 0 then v0' else summedReward 0 v0' deltaV' 0
  where
    summedReward :: Integer -> Integer -> Integer -> Integer -> Integer
    summedReward counter v0 deltaV t = 
      if t < divide v0 deltaV + 1
        then summedReward (counter + rewardFunction v0 deltaV t) v0 deltaV (t+1)
        else counter

-------------------------------------------------------------------------------
-- | Assumes a linear reward of the form, f = v - t*d.
-------------------------------------------------------------------------------
rewardFunction :: Integer -> Integer -> Integer -> Integer
rewardFunction v0 deltaV t = if value >= 0 then value else 0
  where
    value :: Integer
    value = v0 - t*deltaV

-------------------------------------------------------------------------------
-- | Calculates the ending time in unix time for some vestment.
--
--   The reference time is defined inside this function.
-------------------------------------------------------------------------------
calculateEndTime :: Integer -> Integer -> Integer -> Integer
calculateEndTime startNumber lockedPeriod timeUnit = endingTime
  where
  -- This must be some fix point in time.
    timeTilRefEpoch :: Integer
    timeTilRefEpoch = 1660498238433  -- 10:30 am pst 8/14/22

    -- time unit
    lengthOfTime :: Integer
    lengthOfTime = timeUnit

    startingTime :: Integer
    startingTime = timeTilRefEpoch + startNumber*lengthOfTime

    endingTime :: Integer
    endingTime = startingTime + lockedPeriod*lengthOfTime

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
-- | Creates a proper BuiltinByteString type.
-------------------------------------------------------------------------
createBuiltinByteString :: [Integer] -> PlutusV2.BuiltinByteString
createBuiltinByteString intList = flattenBuiltinByteString [ consByteString x emptyByteString | x <- intList]
  where
    flattenBuiltinByteString :: [PlutusV2.BuiltinByteString] -> PlutusV2.BuiltinByteString
    flattenBuiltinByteString []     = emptyByteString 
    flattenBuiltinByteString (x:xs) = appendByteString x (flattenBuiltinByteString xs)

-------------------------------------------------------------------------
-- | Create a proper Address type.
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
    checkVal = PlutusV2.txOutValue x == val -- must be exact

-------------------------------------------------------------------------------
-- | Search each TxOut for an addr and value.
-------------------------------------------------------------------------------
isAddrHolding :: [PlutusV2.TxOut] -> PlutusV2.Address -> Integer -> PlutusV2.CurrencySymbol -> PlutusV2.TokenName -> Bool
isAddrHolding []     _    _   _   _ = False
isAddrHolding (x:xs) addr val pid tkn
  | checkAddr && checkVal = True
  | otherwise             = isAddrHolding xs addr val pid tkn
  where
    checkAddr :: Bool
    checkAddr = PlutusV2.txOutAddress x == addr

    checkVal :: Bool
    checkVal = Value.valueOf (PlutusV2.txOutValue x) pid tkn == val -- must be exact

-------------------------------------------------------------------------------
-- | Count the number of inputs that have datums of any kind.
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
-- | Count the number of outputs that have datums of any kind.
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