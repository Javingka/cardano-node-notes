[github ref](https://developers.cardano.org/docs/integrate-cardano/creating-wallet-faucet/)

### 1. Run node

```bash
cardano-node run \
--topology ./configuration/cardano/testnet-topology.json \
--database-path ./db-test \
--socket-path ./node.socket \
--port 3001 \
--config ./configuration/cardano/testnet-config.json
```
 

### 2. Setup environment variables on .bashrc file

```bash
export CARDANO_NODE_SOCKET_PATH="/home/user/cardano-src/cardano-node/node.socket"
export TESTNET="--testnet-magic 1097911063"
```


### 3. Ask for tip of the synch testnet

```bash
cardano-cli query tip $TESTNET
```

### 4. Create an address. 
You should have a payment address and a staking address where receive staking regards 

#### 4.1 Create payment key pair 

```bash
cardano-cli address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey
```

#### 4.2 Create staking key pair

```bash
cardano-cli stake-address key-gen \
--verification-key-file stake1.vkey \
--signing-key-file stake1.skey
```


#### 4.3 Create payment address at testnet

```bash
cardano-cli address build \
--payment-verification-key-file payment.vkey \
--stake-verification-key-file stake1.vkey \
--out-file payment.addr \
$TESTNET
```

```bash
address1=$(cat payment.addr)
```

[Get fund](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/)
### 5. Check the extract for an address

```bash
cardano-cli query utxo --address $address1 $TESTNET
```



