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
import           HelperFunctions
import           DataTypes
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  
  cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
  git rev 950c4e222086fed5ca53564e642434ce9307b0b9

  cabal-install version 3.6.2.0
  compiled using version 3.6.2.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.7
-}
-------------------------------------------------------------------------------
-- | The token to be distributed.
-------------------------------------------------------------------------------
lockPid :: PlutusV2.CurrencySymbol
lockPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [92, 88, 170, 212, 8, 207, 226, 213, 34, 22, 23, 164, 148, 29, 43, 53, 42, 169, 179, 84, 145, 49, 92, 141, 104, 81, 148, 23] }

lockTkn :: PlutusV2.TokenName
lockTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [79, 110, 101, 86, 101, 114, 121, 76, 111, 110, 103, 83, 116, 114, 105, 110, 103, 70, 111, 114, 84, 101, 115, 116, 67, 111, 110, 116, 114, 97, 99, 116] }

-------------------------------------------------------------------------------
-- | Create the datum type.
-------------------------------------------------------------------------------
data CustomDatumType = Vesting VestingData
PlutusTx.makeIsDataIndexed ''CustomDatumType  [ ( 'Vesting, 0 ) ]

-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Retrieve | 
                          Close
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Retrieve, 0 )
                                                , ( 'Close,    1 )
                                                ]

-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  -- loop all available datums for Vesting
  case datum of
    {- | Datum : Vesting VestingData
      
      Allows a UTxO to be retrieved or closed. All vesting information is contained in
      the datum of the UTxO. A reward may be retrieve if and only if the vesting UTxO
      is returned back to the script under very specific conditions. A UTxO is then closed
      when the vesting is complete.

    -}
    (Vesting vd) ->
      -- datum based variables
      let endTime         = calculateEndTime (cdtStartPoint vd) (cdtLockPeriod vd) (cdtTimeUnit vd)
          vestingUser     = cdtVestingUserPkh vd
          userAddr        = createAddress vestingUser (cdtVestingUserSc vd)
          retrievingValue = calculateRetrieveValue vd
      -- loop all available redeemers for Vesting
      in case redeemer of
        
        {- | Redeemer : Retrieve
      
          Allows a reward to be retrieved from the contract.

          There can only be a single script input and a single output going to the script. On that output going
          to the script, the UTxO must contain the correct return value and datum. The act of vesting is always a choice
          of the vesting user so that user must sign the transaction. The vesting user must recieve their rewards
          via some kind of UTxO inside the transaction and the reward can not be zero. Finally, a vesting user can only
          receive a reward after their locking period.

        -}
        Retrieve -> 
          case getOutboundDatum contTxOutputs (validatingValue - retrievingValue) of   -- get datum from utxo holding that val
            Nothing            -> traceIfFalse "Retrieve:GetOutboundDatum Error" False -- no txout has the correct val
            Just outboundDatum ->
              -- loop all available datums for the Retrieve redeemer
              case outboundDatum of
                
                -- | Go back to the Vesting state for the next vesting phase.
                (Vesting vd') -> do
                  { let retrievingTkn = Value.valueOf retrievingValue lockPid lockTkn
                  ; let a = traceIfFalse "Too Many In/Out"    $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1             -- single input single output
                  ; let b = traceIfFalse "Wrong Tx Signer"    $ ContextsV2.txSignedBy info vestingUser                         -- wallet must sign it
                  ; let c = traceIfFalse "Incorrect Datum"    $ compareVestingData vd vd'                                      -- the datum changes correctly
                  ; let d = traceIfFalse "Vestment Not Paid"  $ isAddrHolding txOutputs userAddr retrievingTkn lockPid lockTkn -- wallet must get the tokens
                  ; let e = traceIfFalse "Value Still Locked" $ isTxOutsideInterval endTime validityInterval                   -- must be outside lock
                  ; let f = traceIfFalse "Reward Is Zero"     $ not $ Value.isZero retrievingValue                             -- reward is non zero
                  ;         traceIfFalse "Retrieve Error"     $ all (==(True :: Bool)) [a,b,c,d,e,f]
                  }

        {- | Redeemer : Close
      
          Allows the vestment UTxO to be closed from the contract.

          There can only be a single script input as the entire value being spent must return to the vesting user.
          Nothing is returning to the script here so no datum checks need to occur. Closing is part of the act of
          vesting so it requires the vesting user to sign the transaction. The vesting user must recieve at least the vestment
          UTxO inside the transaction. A vestment can be closed either when the reward is too large and the there is not enough
          tokens to give, or when the reward is zero. Finally, a vesting user can only receive a reward after their
          locking period.

        -}
        Close -> do
          { let validatingTkn = Value.valueOf validatingValue lockPid lockTkn
          ; let retrievingTkn = Value.valueOf retrievingValue lockPid lockTkn
          ; let a = traceIfFalse "Too Many In/Out"    $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0             -- single input no outputs
          ; let b = traceIfFalse "Wrong Tx Signer"    $ ContextsV2.txSignedBy info vestingUser                         -- wallet must sign it
          ; let c = traceIfFalse "Vestment Not Paid"  $ isAddrGettingPaid txOutputs userAddr validatingValue           -- send back the leftover
          ; let d = traceIfFalse "Funds Are Leftover" $ validatingTkn <= retrievingTkn || Value.isZero retrievingValue -- not enough or leftover
          ; let e = traceIfFalse "UTxO Still Locked"  $ isTxOutsideInterval endTime validityInterval                   -- must be outside lock
          ;         traceIfFalse "Close Error"        $ all (==(True :: Bool)) [a,b,c,d,e]
          }
  -- |
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    validityInterval :: PlutusV2.POSIXTimeRange
    validityInterval = ContextsV2.txInfoValidRange info

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "" -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input

    -------------------------------------------------------------------------------
    -- | Calculate what is going to be rewarded to the user.
    -------------------------------------------------------------------------------
    calculateRetrieveValue :: VestingData -> PlutusV2.Value
    calculateRetrieveValue datum' = Value.singleton lockPid lockTkn (rewardFunction v0 deltaV t)
      where
        -- starting amount
        v0 :: Integer
        v0 = cdtStartingAmount datum'

        -- amount reduced every period
        deltaV :: Integer
        deltaV = cdtDeltaAmount datum'

        -- time increment
        t :: Integer
        t = cdtVestingStage datum'
    
    -------------------------------------------------------------------------------
    -- | Get the inline datum that holds a value from a list of tx outs.
    -------------------------------------------------------------------------------
    getOutboundDatum :: [PlutusV2.TxOut] -> PlutusV2.Value -> Maybe CustomDatumType
    getOutboundDatum []     _ = Nothing
    getOutboundDatum (x:xs) val =
      if traceIfFalse "Incorrect Outbound Value" $ PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> getOutboundDatum xs val -- loop anything without datums
            (PlutusV2.OutputDatumHash _) -> getOutboundDatum xs val -- loop anything with embedded datums
            
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> getOutboundDatum xs val
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
        
        -- just loop if bad value
        else getOutboundDatum xs val
-- end of mkValidator

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
