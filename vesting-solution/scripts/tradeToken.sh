#!/bin/bash
set -e

# SET UP VARS HERE
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
TESTNET_MAGIC=$(cat data/testnet.magic)

# Addresses
vestor_address=$(cat wallets/buyer-wallet/payment.addr)
issuer_address=$(cat wallets/seller-wallet/payment.addr)

# Token Information
policy_id=$(cat ../start_info.json | jq -r .pid)
token_name=$(cat ../start_info.json | jq -r .tkn)
amount=5500
asset="${amount} ${policy_id}.${token_name}"
# asset="${amount} ${policy_id}.${token_hex}"

min_utxo=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-hash-value 42 \
    --tx-out="${issuer_address} ${asset}" | tr -dc '0-9')
token_to_be_traded="${issuer_address} + ${min_utxo} + ${asset}"
# token_to_be_traded="${issuer_address} + 250000000"

echo -e "\nTrading A Token:\n" ${token_to_be_traded}
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${TESTNET_MAGIC} \
    --address ${vestor_address} \
    --out-file tmp/vestor_utxo.json

TXNS=$(jq length tmp/vestor_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${vestor_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/vestor_utxo.json)
HEXTXIN=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${vestor_address} \
    --tx-in ${HEXTXIN} \
    --tx-out="${token_to_be_traded}" \
    --testnet-magic ${TESTNET_MAGIC})

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/buyer-wallet/payment.skey \
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