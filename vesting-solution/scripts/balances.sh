#!/usr/bin/bash
set -e

TESTNET_MAGIC=$(cat data/testnet.magic)

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
script_path="../vesting-contract/vesting-contract.plutus"

script_address=$(${cli} address build --payment-script-file ${script_path} --testnet-magic ${TESTNET_MAGIC})
issuer_address=$(cat wallets/seller-wallet/payment.addr)
vestor_address=$(cat wallets/buyer-wallet/payment.addr)
reference_address=$(cat wallets/reference-wallet/payment.addr)
collat_address=$(cat wallets/collat-wallet/payment.addr)


echo
${cli} query protocol-parameters --testnet-magic ${TESTNET_MAGIC} --out-file tmp/protocol.json
${cli} query tip --testnet-magic ${TESTNET_MAGIC} | jq

echo -e "\n\nScript Address:" ${script_address}
${cli} query utxo --address ${script_address} --testnet-magic ${TESTNET_MAGIC}

echo -e "\n\nVestor Address:" ${vestor_address}
${cli} query utxo --address ${vestor_address} --testnet-magic ${TESTNET_MAGIC}

echo -e "\n\nIssuer Address:" ${issuer_address}
${cli} query utxo --address ${issuer_address} --testnet-magic ${TESTNET_MAGIC}

echo -e "\n\nReference Address:" ${reference_address}
${cli} query utxo --address ${reference_address} --testnet-magic ${TESTNET_MAGIC}

echo -e "\n\nCollat Address:" ${collat_address}
${cli} query utxo --address ${collat_address} --testnet-magic ${TESTNET_MAGIC}