#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
TESTNET_MAGIC=$(cat data/testnet.magic)


minter_address=$(cat wallets/seller-wallet/payment.addr)
receiver_address=$(cat wallets/buyer-wallet/payment.addr)

policy_id=$(cardano-cli transaction policyid --script-file policy/policy.script)

token_name="OneVeryLongStringForTestContract"

hex_token_name=$(echo -n ${token_name} | xxd -b -ps -c 80 | tr -d '\n')
asset="5500 ${policy_id}.${hex_token_name}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-hash-value 42 \
    --tx-out="${minter_address} ${asset}" | tr -dc '0-9')
echo $min_utxo
token_to_be_minted="${receiver_address} + ${min_utxo} + ${asset}"
echo -e "\nMinting A Token:\n" ${token_to_be_minted}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${TESTNET_MAGIC} \
    --address ${minter_address} \
    --out-file tmp/minter_utxo.json

TXNS=$(jq length tmp/minter_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${minter_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/minter_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --alonzo-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${minter_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${token_to_be_minted}" \
    --mint="${asset}" \
    --minting-script-file policy/policy.script \
    --witness-override ${TESTNET_MAGIC} \
    --testnet-magic ${TESTNET_MAGIC})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file policy/policy.skey  \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${TESTNET_MAGIC}
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file tmp/tx.signed