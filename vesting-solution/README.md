# Vesting Contract

The vesting contract is initialized by a specific token defined in the start_info.json. The example below is the start information for the test DRIP token.

```json
{
    "pid": "698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d",
    "tkn": "7444524950"
}
```

The contract can then be compiled with the complete_build.sh script.


```bash
# bash complete_build.sh
./complete_build.sh
```

# Use

Each UTxO represents a user's vesting solution for some predefined initial conditions contained in the datum.

```hs
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
```

The vesting stage represents an integer counter of how many times a user has vested. This information is used in the reward calculation function along with the starting amount and delta amount. The vesting user's public key hash and stake hash are contained in the datum for proper address creation. The staking credential may be left blank if just a payment addresses is required. The starting amount is the initial reward a user will recieve upon their first vestment and the delta amount is the decrease in the reward per vesting stage. If the delta amount is non-zero then the reward function will decrease to zero in some finite amount of vesting stages. The start point, lock period, and time unit are the three variables that define how the UTxO responds to time. The starting point is the integer number of time units from the reference point. The lockperiod is the number of time units the UTxO will be unspendable. This information allows the UTxO to be locked periodically.

### Gitbook
[Write Up On The Math For Linear Vesting](https://logicalmechanism.gitbook.io/linear-vesting/)

## Scripts

There are test example scripts in the scripts folder. It assumes there exists a tmp and wallets folder. The tagged releases do work on testnet. The scripts are designed to vest the test DRIP token over many vesting stages on a 5 min vesting phase.

```
wallets ->
    buyer-wallet
    collat-wallet
    delegator-wallet
    profit-wallet
    reference-wallet
    seller-wallet
```

The path_to_socket.sh file is the path to the node socket for testnet. Change it accordingly. The get_pkh.sh file takes in an address as its only input and outputs the payment public key hash. This is useful for setting up datums.

# Notes

The contract is designed to be on PlutusV2.

### Versions Used
```
  cardano-cli 1.35.2 - linux-x86_64 - ghc-8.10
  git rev 7612a245a6e2c51d0f1c3e0d65d7fe9363850043

  cabal-install version 3.6.2.0
  compiled using version 3.6.2.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.7
```