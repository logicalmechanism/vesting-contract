#!/bin/bash
# set -e

# Complete Build
echo -e "\033[1;35m Starting... \033[0m"

echo -e "\033[1;35m Data Generate \033[0m"
# starter nft data
python3 -c "import binascii;a=$(cat start_info.json | jq .pid);s=binascii.unhexlify(a);print([x for x in s])" > start.pid
python3 -c "import binascii;a=$(cat start_info.json | jq .tkn);s=binascii.unhexlify(a);print([x for x in s])" > start.tkn

# multisig
python3 -c "import binascii;a=$(cat start_info.json | jq .key1);s=binascii.unhexlify(a);print([x for x in s])" > multisig1.key
python3 -c "import binascii;a=$(cat start_info.json | jq .key2);s=binascii.unhexlify(a);print([x for x in s])" > multisig2.key
python3 -c "import binascii;a=$(cat start_info.json | jq .key3);s=binascii.unhexlify(a);print([x for x in s])" > multisig3.key

# adds in the multsig into the contract
python3 -c "from update_contracts import changeMultiPkh;changeMultiPkh('./vesting-contract/src/VestingContract.hs', './vesting-contract/src/VestingContract-new.hs', $(cat multisig1.key), $(cat multisig2.key), $(cat multisig3.key))"
mv ./vesting-contract/src/VestingContract-new.hs ./vesting-contract/src/VestingContract.hs

# Adds in the locking token into the contract.
python3 -c "from update_contracts import changeStartLockPid;changeStartLockPid('./vesting-contract/src/VestingContract.hs', './vesting-contract/src/VestingContract-new.hs', $(cat start.pid))"
mv ./vesting-contract/src/VestingContract-new.hs ./vesting-contract/src/VestingContract.hs
python3 -c "from update_contracts import changeStartLockTkn;changeStartLockTkn('./vesting-contract/src/VestingContract.hs', './vesting-contract/src/VestingContract-new.hs', $(cat start.tkn))"
mv ./vesting-contract/src/VestingContract-new.hs ./vesting-contract/src/VestingContract.hs

# build
echo -e "\033[1;35m Build Contract \033[0m"
cd vesting-contract
#
rm validator.bytes
rm validator.hash
cabal build -w ghc-8.10.7
cabal run vesting-contract
cardano-cli address build --payment-script-file vesting-contract.plutus --testnet-magic 2 --out-file validator.addr
cardano-cli transaction policyid --script-file vesting-contract.plutus > validator.hash
python3 -c "import binascii;a='$(cat validator.hash)';s=binascii.unhexlify(a);print([x for x in s])" > validator.bytes

echo -e "\033[1;36m Validator Addr: $(cat validator.addr) \033[0m"
echo -e "\033[1;36m Validator Hash: $(cat validator.hash) \033[0m"
echo -e "\033[1;36m Validator Bytes: $(cat validator.bytes) \033[0m"

echo -e "\033[1;35m Hash Outputs \033[0m"
cd ..
#
rm hash.hashes
rm final.check
#
find . -name '*.hash' -type f -exec sha256sum {} \; > hash.hashes
find . -name '*.hashes' -type f -exec sha256sum {} \; > final.check

echo -e "\033[1;32m $(sha256sum final.check) \033[0m"