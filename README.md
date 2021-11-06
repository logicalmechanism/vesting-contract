```bash
# cd cardano-node
./testnet-node-local/bin/cardano-node-testnet
```

```bash
# cd bash_scripts
CARDANO_NODE_SOCKET_PATH=../cardano-node/state-node-testnet/node.socket
../cardano-node/cardano-cli-testnet/bin/cardano-cli query tip --testnet-magic 1097911063
```


```bash
cabal clean
cabal build -w ghc-8.10.4
cabal run vesting-contract
cp vesting_contract.plutus ../../compiled_plutus_scripts/vesting_contract_rev0.plutus
echo "done"
```


```bash
echo "# testing" >> README.md
git init
git add README.md
git commit -m "first commit"
git branch -M main
git remote add origin https://github.com/logicalmechanism/vesting-contract.git
git push -u origin main
```