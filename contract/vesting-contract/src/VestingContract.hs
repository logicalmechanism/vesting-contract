{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module VestingContract
  ( vestingContractScript
  , vestingContractScriptShortBs
  ) where

import           Codec.Serialise

import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS

import           Prelude                   hiding (($))

import           Ledger                    hiding (singleton)
import qualified Ledger.Typed.Scripts      as Scripts

import           Cardano.Api.Shelley       (PlutusScript (..), PlutusScriptV1)

import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless)

import qualified Plutus.V1.Ledger.Ada      as Ada
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import qualified Plutus.V1.Ledger.Value    as Value

{- |
  Author   : The Ancient Kraken
  Copyright: 2021
  Version  : Rev 0
-}

-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data VestingContractParams = VestingContractParams
  { vcMajorityParam :: !Integer
  -- ^ The number of keys that determines the majority.
  , vcPolicyID      :: !CurrencySymbol
  -- ^ The policy id of the vesting token.
  , vcTokenName     :: !TokenName
  -- ^ The token name of the vesting token.
  }
PlutusTx.makeLift ''VestingContractParams


-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------

data CustomDatumType = CustomDatumType 
  { cdtVestAmount      :: !Integer
  -- ^ The amount of lovelace the user is allowed to extract.
  , cdtVestDeadline    :: !Integer
  -- ^ Fund must be retrieved after this deadline.
  , cdtVestingUserPKH  :: !PubKeyHash
  -- ^ The public key hash of the receiver.
  , cdtVestingGroupPKH :: ![PubKeyHash]
  -- ^ A list public key hashes of everyone who is vesting with the contract.
  , cdtTreasuryPKH     :: !PubKeyHash
  -- ^ The public key hash of the treasury wallet.
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift ''CustomDatumType


-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------

data CustomRedeemerType = CustomRedeemerType 
  { crtAction    :: !Integer
  -- ^ The action determines which type of validation to use in the contract.
  }
PlutusTx.unstableMakeIsData ''CustomRedeemerType
PlutusTx.makeLift ''CustomRedeemerType


-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator vc)
  where 
    vc = VestingContractParams
      { vcMajorityParam = 3
      , vcPolicyID      = "5243f6530c3507a3ed1217848475abb5ec0ec122e00c82e878ff2292"
      , vcTokenName     = "TokenC"
      }


-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: VestingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator vc datum redeemer context 
  | checkRedeemer = True
  | otherwise     = False
    where
      -------------------------------------------------------------------------
      -- | Use the redeemer to switch validators.
      -------------------------------------------------------------------------
      
      checkRedeemer :: Bool
      checkRedeemer
        | action P.== 0 = retrieve  -- | Retrieve vesting amount
        | action P.== 1 = remove    -- | Remove user from vesting contract.
        | otherwise     = False
      
      action :: Integer
      action = (crtAction redeemer)

      -------------------------------------------------------------------------
      -- | Different Types of Validators Here
      -------------------------------------------------------------------------
      
      -- | Put all the retrieve functions together here.
      retrieve :: Bool
      retrieve = do
        { let a = True
        ; let b = True
        ; P.all (P.==(True :: Bool)) [a,b]
        }
      
      -- | Put all the retrieve functions together here.
      remove :: Bool
      remove = do
        { let a = True
        ; let b = True
        ; P.all (P.==(True :: Bool)) [a,b]
        }

      -------------------------------------------------------------------------
      -- | Check Some Condition Functions Here
      -------------------------------------------------------------------------

      -- put functions right here

      -------------------------------------------------------------------------
      -- | Script Info and TxOutputs
      -------------------------------------------------------------------------
      
      -- The script info.
      info :: TxInfo
      info = scriptContextTxInfo context

      -- All the current outputs.
      currentTxOutputs :: [TxOut]
      currentTxOutputs = txInfoOutputs info

      -- All the outputs going back to the script.
      scriptTxOutputs  :: [TxOut]
      scriptTxOutputs  = Contexts.getContinuingOutputs context

      -------------------------------------------------------------------------
      -- | Different Types of Vesting Data Here
      -------------------------------------------------------------------------
      
      tokenValue :: Value
      tokenValue = case Contexts.findOwnInput context of
        Nothing     -> traceError "No Input to Validate."
        Just input  -> txOutValue $ txInInfoResolved $ input

      vestingAmount :: Integer
      vestingAmount = cdtVestAmount datum

      vestingValue :: Value
      vestingValue = Ada.lovelaceValueOf vestingAmount

      valueAmount :: Integer
      valueAmount = Value.valueOf vestingValue (vcPolicyID vc) (vcTokenName vc)

      vestingPKH :: PubKeyHash
      vestingPKH = cdtVestingUserPKH datum

      vestingAddr :: Address
      vestingAddr = pubKeyHashAddress vestingPKH

      vestingGroup :: [PubKeyHash]
      vestingGroup = cdtVestingGroupPKH datum

      treasuryPKH :: PubKeyHash
      treasuryPKH = cdtTreasuryPKH datum

      treasuryAddr :: Address
      treasuryAddr = pubKeyHashAddress treasuryPKH

      -------------------------------------------------------------------------
      -- | Helper Functions Here
      -------------------------------------------------------------------------

      -- | Check if a signee has signed the pending transaction.
      checkTxSigner :: PubKeyHash -> Bool
      checkTxSigner signee = Contexts.txSignedBy info signee  -- Not Working as of 1.30.1

      -- -- | Search each TxOut for the correct address and value.
      checkTxOutForValueAtAddress :: [TxOut] -> Address -> Value -> Bool
      checkTxOutForValueAtAddress [] _addr _val = False
      checkTxOutForValueAtAddress (x:xs) addr val
        | ((txOutAddress x) P.== addr) P.&& ((txOutValue x) P.== val) = True
        | otherwise                                                   = checkTxOutForValueAtAddress xs addr val
      
      -- | Search each TxOut for the correct value.
      checkTxOutForValue :: [TxOut] -> Value -> Bool
      checkTxOutForValue [] _val = False
      checkTxOutForValue (x:xs) val
        | (txOutValue x) P.== val = True
        | otherwise = checkTxOutForValue xs val

      
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------

data Typed
instance Scripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType


-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------

typedValidator :: VestingContractParams -> Scripts.TypedValidator Typed
typedValidator vc = Scripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode vc)
  $$(PlutusTx.compile  [|| wrap        ||])
    where
      wrap = Scripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer


-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------

script :: Plutus.Script
script = Plutus.unValidatorScript validator

vestingContractScriptShortBs :: SBS.ShortByteString
vestingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

vestingContractScript :: PlutusScript PlutusScriptV1
vestingContractScript = PlutusScriptSerialised vestingContractScriptShortBs