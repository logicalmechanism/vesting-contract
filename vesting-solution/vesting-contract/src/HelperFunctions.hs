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
  ( rewardFunction
  , calculateEndTime
  , totalReward
  ) where
import           PlutusTx.Prelude 
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | For Testing Only.
-------------------------------------------------------------------------------
totalReward :: Integer -> Integer -> Integer
totalReward v0' deltaV' = if deltaV' == 0 then v0' else summedReward 0 v0' deltaV' 0
  where
    summedReward :: Integer -> Integer -> Integer -> Integer -> Integer
    summedReward counter v0 deltaV t = 
      if t < (divide v0 deltaV) + 1
        then summedReward (counter + rewardFunction v0 deltaV t) v0 deltaV (t+1)
        else counter
-------------------------------------------------------------------------------
-- | Assumes a linear reward of the form, f = v - t*d.
-------------------------------------------------------------------------------
rewardFunction :: Integer -> Integer -> Integer -> Integer
rewardFunction v0 deltaV t = if value >= 0 then value else 0
  where
    value :: Integer
    value = v0 - (t * deltaV)
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
    timeTilRefEpoch = 1660498238433  -- 10:30 am pst 8/14/22 CHANGE THIS LATER

    -- time unit
    lengthOfTime :: Integer
    lengthOfTime = timeUnit

    startingTime :: Integer
    startingTime = timeTilRefEpoch + (startNumber * lengthOfTime)

    endingTime :: Integer
    endingTime = startingTime + (lockedPeriod * lengthOfTime)