#!/bin/bash
set -e

# Paths
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)

TESTNET_MAGIC=$(cat data/testnet.magic)

# Addresses
script_path="../vesting-contract/vesting-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${TESTNET_MAGIC})
#
issuer_address=$(cat wallets/seller-wallet/payment.addr)
#
vestor_address=$(cat wallets/buyer-wallet/payment.addr)

# Token Information
policy_id=$(cat ../start_info.json | jq -r .pid)
token_name=$(cat ../start_info.json | jq -r .tkn)
amount=5500
sc_asset="${amount} ${policy_id}.${token_name}"

# minimum ada to get in
min_value=$(${cli} transaction calculate-min-required-utxo \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --tx-out="${script_address} ${sc_asset}" \
    --tx-out-inline-datum-file data/current_datum.json | tr -dc '0-9')
lock_value=5000000
sc_address_out="${script_address} + ${lock_value} + ${sc_asset}"
# sc_address_out="${script_address} + ${lock_value}"

echo "Script OUTPUT: "${sc_address_out}
#
# exit
#
echo -e "\033[0;36m Getting Buyer UTxO Information \033[0m"
# get utxo
${cli} query utxo \
    --testnet-magic ${TESTNET_MAGIC} \
    --address ${issuer_address} \
    --out-file tmp/issuer_utxo.json

# transaction variables
TXNS=$(jq length tmp/issuer_utxo.json)
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/issuer_utxo.json)
issuer_tx_in=${TXIN::-8}

echo -e "\033[0;36m Building Tx \033[0m"
# build tx
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${issuer_address} \
    --tx-in ${issuer_tx_in} \
    --tx-out="${sc_address_out}" \
    --tx-out-inline-datum-file data/current_datum.json \
    --testnet-magic ${TESTNET_MAGIC})

IFS=':' read -ra VALUE <<< "$FEE"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee \033[0m" ${FEE}
#
exit
#
echo -e "\033[0;36m Signing Tx \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${TESTNET_MAGIC}
#
# exit
#
echo -e "\033[0;36m Submitting Tx \033[0m"
${cli} transaction submit \
    --testnet-magic ${TESTNET_MAGIC} \
    --tx-file tmp/tx.signed