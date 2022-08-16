#!/bin/bash
set -e

# Paths
export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../vesting-contract/vesting-contract.plutus"

# Addresses
script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
vestor_address=$(cat wallets/buyer-wallet/payment.addr)
vestor_pkh=$(cardano-cli address key-hash --payment-verification-key-file wallets/buyer-wallet/payment.vkey)

# Token Information
policy_id=$(cat ../start_info.json | jq -r .pid)
token_name=$(cat ../start_info.json | jq -r .tkn)
amount=100
asset="5400 ${policy_id}.${token_name}"
sc_asset="${amount} ${policy_id}.${token_name}"

# minimum ada to get in
sc_min_value=$(${cli} transaction calculate-min-required-utxo \
    --protocol-params-file tmp/protocol.json \
    --tx-out-datum-embed-file data/current_datum.json \
    --tx-out="${vestor_address} ${sc_asset}" | tr -dc '0-9')

vestor_address_change="${vestor_address} + ${sc_min_value} + ${asset}"
vestor_address_out="${vestor_address} + ${sc_min_value} + ${sc_asset}"
echo "Issuer OUTPUT: "${vestor_address_out}
#
# exit
#
echo -e "\033[0;36m Gathering UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic 1097911063 \
    --address ${vestor_address} \
    --out-file tmp/vestor_utxo.json

TXNS=$(jq length tmp/vestor_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${vestor_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/vestor_utxo.json)
CTXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in-collateral"' tmp/vestor_utxo.json)
collateral_tx_in=${CTXIN::-19}
vestor_tx_in=${TXIN::-8}

echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic 1097911063 \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "$TXNS" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

script_ref_utxo=$(cardano-cli transaction txid --tx-file tmp/tx-reference-utxo.signed)
# collat info
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)
collat_utxo="87a43ee3889f827356a23a7459ef5f9eaf843880da1996d1b68595fb4171f63c" # in collat wallet

slot=$(${cli} query tip --testnet-magic 1097911063 | jq .slot)
current_slot=$(($slot - 1))
final_slot=$(($slot + 750))

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --invalid-before ${current_slot} \
    --invalid-hereafter ${final_slot} \
    --out-file tmp/tx.draft \
    --change-address ${vestor_address} \
    --tx-in ${vestor_tx_in} \
    --tx-in-collateral="${collat_utxo}#0" \
    --tx-in ${script_tx_in}  \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/close_redeemer.json \
    --tx-out="${vestor_address_out}" \
    --tx-out="${vestor_address_change}" \
    --required-signer-hash ${vestor_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic 1097911063)

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
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic 1097911063
#
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tmp/tx.signed

ft=1
variable=${ft}; jq -r --argjson variable $variable '.fields[0].fields[0].int=$variable' data/next_datum.json > data/next_datum-new.json
mv data/next_datum-new.json data/next_datum.json

nChange=4500
nReward=1000
variable=$((nChange)); jq -r --argjson variable "$variable" '.change=$variable' data/price.data > data/price-new.data
mv data/price-new.data data/price.data
variable=$((nReward)); jq -r --argjson variable "$variable" '.reward=$variable' data/price.data > data/price-new.data
mv data/price-new.data data/price.data