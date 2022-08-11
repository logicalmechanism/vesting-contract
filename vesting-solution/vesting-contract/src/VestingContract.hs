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
import           Plutus.Script.Utils.V2.Scripts as Utils
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
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType {}
PlutusTx.unstableMakeIsData ''CustomDatumType
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType =  CustomRedeemerType {}
PlutusTx.unstableMakeIsData ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator _ _ _ = True -- Always Succeeds
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
