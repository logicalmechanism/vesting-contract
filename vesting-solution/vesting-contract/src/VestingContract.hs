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
module VestingContract
  ( vestingContractScript
  , vestingContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import qualified Plutus.V1.Ledger.Value         as Value
import           Plutus.Script.Utils.V2.Scripts as Utils
import HelperFunctions
import DataTypes
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  
  cardano-cli 1.35.2 - linux-x86_64 - ghc-8.10
  git rev 7612a245a6e2c51d0f1c3e0d65d7fe9363850043

  cabal-install version 3.6.2.0
  compiled using version 3.6.2.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.7
-}
-------------------------------------------------------------------------------
-- | The token to be distributed.
-------------------------------------------------------------------------------
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [105, 138, 110, 160, 202, 153, 243, 21, 3, 64, 114, 175, 49, 234, 172, 110, 193, 31, 232, 85, 141, 63, 72, 233, 119, 90, 171, 157] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [116, 68, 82, 73, 80] }
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Retrieve | 
                          Close    |
                          Debug -- remove in production
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Retrieve, 0 )
                                                , ( 'Close,    1 )
                                                , ( 'Debug,    2 ) -- remove in production
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    Retrieve -> 
      case getOutboundDatum contTxOutputs (validatingValue - retrievingValue) of
        Nothing            -> traceIfFalse "Retrieve:GetOutboundDatum Error" False
        Just outboundDatum -> do
          { let retrievingTkn = Value.valueOf retrievingValue lockPid lockTkn
          ; let a = traceIfFalse "Input / Output Error"      $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1             -- single tx going in
          ; let b = traceIfFalse "Incorrect Tx Signer Error" $ ContextsV2.txSignedBy info vestingUser                         -- wallet must sign it
          ; let c = traceIfFalse "Datum Equality Error"      $ datum == outboundDatum                                         -- the datum changes correctly
          ; let d = traceIfFalse "Vestment Not Paid Error"   $ isAddrHolding txOutputs userAddr retrievingTkn lockPid lockTkn -- wallet must get the utxo
          ; let e = traceIfFalse "The Value Is Still Locked" $ isTxOutsideInterval endTime validityInterval                   -- must be outside lock
          ; let f = traceIfFalse "Not Enough Reward Error"   $ not $ Value.isZero retrievingValue                             -- cant be non zero reward
          ;         traceIfFalse "Error: Retrieve Failure"   $ all (==(True :: Bool)) [a,b,c,d,e,f]
          }
    Close -> do
      { let validatingTkn = Value.valueOf validatingValue lockPid lockTkn
      ; let retrievingTkn = Value.valueOf retrievingValue lockPid lockTkn
      ; let a = traceIfFalse "Single Script Only"           $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0             -- single input no outputs
      ; let b = traceIfFalse "Incorrect Tx Signer Error"    $ ContextsV2.txSignedBy info vestingUser                         -- wallet must sign it
      ; let c = traceIfFalse "Funds Not Being Retrieved"    $ isAddrGettingPaid txOutputs userAddr validatingValue           -- send the leftover
      ; let d = traceIfFalse "Funds Are Left To Vest"       $ validatingTkn <= retrievingTkn || Value.isZero retrievingValue -- not enough or leftover
      ; let e = traceIfFalse "The Value Is Still Locked"    $ isTxOutsideInterval endTime validityInterval                   -- must be outside lock
      ;         traceIfFalse "Error: Close Failure" $ all (==(True :: Bool)) [a,b,c,d,e]
      }
    Debug -> True -- remove in production
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    validityInterval :: PlutusV2.POSIXTimeRange
    validityInterval = ContextsV2.txInfoValidRange info

    endTime :: Integer
    endTime = calculateEndTime (cdtStartPoint datum) (cdtLockPeriod datum) (cdtTimeUnit datum)

    -- inputs outputs
    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    -- users
    vestingUser :: PlutusV2.PubKeyHash
    vestingUser = cdtVestingUserPkh datum

    userAddr :: PlutusV2.Address
    userAddr = createAddress vestingUser (cdtVestingUserSc datum)

    -- what is currently being spent
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "No Input to Validate." -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input

    -- what is going to be rewarded
    retrievingValue :: PlutusV2.Value
    retrievingValue = Value.singleton lockPid lockTkn (rewardFunction v0 deltaV t)
      where
        -- starting amount
        v0 :: Integer
        v0 = cdtStartingAmount datum

        -- amount reduced every period
        deltaV :: Integer
        deltaV = cdtDeltaAmount datum

        -- time increment
        t :: Integer
        t = cdtVestingStage datum
    
    getOutboundDatum :: [PlutusV2.TxOut] -> PlutusV2.Value -> Maybe CustomDatumType
    getOutboundDatum []     _ = Nothing
    getOutboundDatum (x:xs) val =
      if traceIfFalse "Incorrect Outbound Value" $ PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            -- datumless
            PlutusV2.NoOutputDatum -> getOutboundDatum xs val
            -- inline datum
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> getOutboundDatum xs val
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
            -- embedded datum
            (PlutusV2.OutputDatumHash dh) -> 
              case ContextsV2.findDatum dh info of
                Nothing                  -> getOutboundDatum xs val
                Just (PlutusV2.Datum d') -> 
                  case PlutusTx.fromBuiltinData d' of
                    Nothing       -> getOutboundDatum xs val
                    Just embedded -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType embedded
        else getOutboundDatum xs val
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

vestingContractScriptShortBs :: SBS.ShortByteString
vestingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

vestingContractScript :: PlutusScript PlutusScriptV2
vestingContractScript = PlutusScriptSerialised vestingContractScriptShortBs
