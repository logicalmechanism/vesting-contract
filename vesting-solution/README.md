# A Vesting Contract

```
The project is being released under Apache License 2.0 (Apache-2.0)
so you can do what you like with this repo, as long as you include
the required notices. 
```

Each contract is compiled specifically for a single token vestment solution. This allows a single contract to handle an entire vesting group where each utxo represents a member of the vesting group. This logic allows for a nice seperation of assets and amazing customization on vesting user level. The contract provides the functionality for flat-rate or linear decreasing vesting.

The contract uses arbitrary time units. A vestment starts at t0 from the turn of reference time r0. Each vestment is given out to a vestor at some fix interval, deltaT. For example, a vestment contract can start on r0=Jan 5 2022, t0=4, and gives out rewards every 5 days, deltaT=5, then when a vestor successfull retrieves their vestment for the vesting period the time parameters are updated by adding deltaT to t0 and creating a new time parameter. This process will repeat until all the funds have been retrieved.

The vesting payouts do roll over due to the style of the time parameters in this contract.

The reward parameters allow for a constant or linearly decreasing reward function. Each reward payout follow the equation R = v0 - t * deltaV. The variable t is the vesting stage, an integer representing the total number of vestor payouts. The term v0 is the intial value of the reward and deltaV is the decrease to the reward over time. Some initial calculations must be done to ensure correct parameters but the contract is designed to account for bat maths. The function was chosen to model a typical return curve that decays over time but still allows for a user to get constant rewards if required by the vesting group.


# Cloning the repo

The vesting solution is contained in the Logical Mechanism's vesting-contract repository. Clone the repo and checkout the most recent tagged release.

```bash
git clone https://github.com/logicalmechanism/vesting-contract.git
cd vesting-contract
git fetch --all
git checkout $(curl -s https://api.github.com/repos/logicalmechanism/vesting-contract/releases/latest | jq -r .tag_name)
```
This will be the most stable version of the smart contract.

# Building

The vesting contract is initialized by a specific token defined in the start_info.json inside the parent folder. The example below is the start information for the test DRIP token.

```json
{
    "pid": "698a6ea0ca99f315034072af31eaac6ec11fe8558d3f48e9775aab9d",
    "tkn": "7444524950"
}
```

The contract can then be compiled with the complete_build.sh script. This is an automation script for compiling the smart contract.


```bash
# bash complete_build.sh
./complete_build.sh
```

This will auto produce all required files for compiling and output a hash at the end for easy verification. It is recommended to use the complete build function.

Inside the vesting-contract folder exist build files for individual builds of the source code. For local testing please use the repl.

```bash
cabal repl vesting-contract.cabal
```


# Use

Each UTxO represents a user's vesting solution for some predefined initial conditions contained in the datum.

```hs
data VestingData = VestingData
  { cdtVestingStage   :: Integer
  -- ^ The vesting stage determines the deadline and reward.
  , cdtVestingUserPkh :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the vestor.
  , cdtVestingUserSc  :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the vestor.
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

There are test example scripts in the scripts folder. It assumes there exists a tmp and wallets folder. The wallet folder structure is shown below. Test wallets are not provided with the repo. The tagged releases do work on testnet. The scripts are designed to vest the test DRIP token over many vesting stages on a 5 min vesting phase.

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

The general flow is setting up a vestment UTxO for a user then that user can vest their vestment from the contract. Upon completion of their vestment, the user may close their vestment UTxO and complete the vestment interaction.

# Notes

The contract is designed to be on PlutusV2. There will not be any PlutusV1 back ports. This project is an applied continuation of the fund7 project catalyst proposal [A Community Vesting Dapp](https://cardano.ideascale.com/c/idea/382448). The voting aspect is not required for this application and the project has been strip down to just the vesting portion. The majority of the vesting logic remained but some PlutusV2 refinements have been applied.

### Versions Used
```
  cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
  git rev 950c4e222086fed5ca53564e642434ce9307b0b9

  cabal-install version 3.6.2.0
  compiled using version 3.6.2.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.7
```