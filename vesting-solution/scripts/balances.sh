#!/usr/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../vesting-contract/vesting-contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic 1097911063)
issuer_address=$(cat wallets/seller-wallet/payment.addr)
vestor_address=$(cat wallets/buyer-wallet/payment.addr)
reference_address=$(cat wallets/reference-wallet/payment.addr)

echo
${cli} query protocol-parameters --testnet-magic 1097911063 --out-file tmp/protocol.json
${cli} query tip --testnet-magic 1097911063 | jq

echo -e "\n\nScript Address:" ${script_address}
${cli} query utxo --address ${script_address} --testnet-magic 1097911063

echo -e "\n\nVestor Address:" ${vestor_address}
${cli} query utxo --address ${vestor_address} --testnet-magic 1097911063

echo -e "\n\nIssuer Address:" ${issuer_address}
${cli} query utxo --address ${issuer_address} --testnet-magic 1097911063

echo -e "\n\nReference Address:" ${reference_address}
${cli} query utxo --address ${reference_address} --testnet-magic 1097911063