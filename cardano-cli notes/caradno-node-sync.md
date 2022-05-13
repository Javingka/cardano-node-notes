github# ref: https://developers.cardano.org/docs/integrate-cardano/creating-wallet-faucet/

# run
cardano-node run \
--topology ./configuration/cardano/testnet-topology.json \
--database-path ./db-test \
--socket-path ./node.socket \
--port 3001 \
--config ./configuration/cardano/testnet-config.json 

# Then setup environment variables on .bashrc file
export CARDANO_NODE_SOCKET_PATH="/home/javingka/cardano-src/cardano-node/node.socket"
export TESTNET="--testnet-magic 1097911063"

# ask for tip of the synch testnet
cardano-cli query tip $TESTNET

# 3 Create an address. you have a payment address and a staking address where receive staking regards 

# 3.1 Create payment keys 
cardano-cli address key-gen \
> --verification-key-file payment.vkey \
> --signing-key-file payment.skey

#3.2 Create staking keys
cardano-cli stake-address key-gen --verification-key-file stake1.vkey --signing-key-file stake1.skey

# Create payment address at testnet
cardano-cli address build \
> --payment-verification-key-file payment.vkey \
> --stake-verification-key-file stake1.vkey \
> --out-file payment.addr \
> $TESTNET

# Create a second address
cardano-cli address key-gen \
> --verification-key-file payment2.vkey \
> --signing-key-file payment2.skey
cardano-cli stake-address key-gen --verification-key-file stake2.vkey --signing-key-file stake2.skey

cardano-cli address build \
> --payment-verification-key-file payment2.vkey \
> --stake-verification-key-file stake2.vkey \
> --out-file payment_2.addr $TESTNET

# Create a third address
cardano-cli address key-gen \
> --verification-key-file payment12.vkey \
> --signing-key-file payment.skey


#4 Got founded at https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

#% Update bashrc file with address variables

# Check the extract for an address
cardano-cli query utxo --address $address1 $TESTNET



