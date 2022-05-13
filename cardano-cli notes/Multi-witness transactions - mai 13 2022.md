[link to crresponding post on developers.cardano platform](https://developers.cardano.org/docs/integrate-cardano/multi-witness-transactions-cli)

### 1. Build 2 Addresses

```bash
cardano-cli address key-gen \
--verification-key-file paymentAdd2.vkey \
--signing-key-file paymentAdd2.skey
```

```bash
cardano-cli stake-address key-gen \
--verification-key-file stakeAdd2.vkey \
--signing-key-file stakeAdd2.skey
```

```bash
cardano-cli address build \
--payment-verification-key-file paymentAdd1.vkey \
--stake-verification-key-file stakeAdd1.vkey \
--out-file paymentAddress1.addr \
$TESTNET
```

Make sure you have credits on each acddress, use [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) to send Ada to one of the Address. Be aware faucet can send only 1 time each 24 hours from the same IP addreess.

Check the UTXOs availables in those Address. 

##### Checking Address 1:
```bash
cardano-cli query utxo --address $(cat paymentAddress1.addr) $TESTNET
```

response should be like this: [^1]
```bash
        TxHash                    TxIx        Amount
----------------------------------------------------------------------------------
a4f6fe6d3cd39ee15f.....69c75602     0        200000000 lovelace + TxOutDatumNone
```

##### Checking Address 2:
```bash
cardano-cli query utxo --address $(cat paymentAddress2.addr) $TESTNET
```

response should be like this: [^1] 
```bash
     TxHash                        TxIx        Amount
-------------------------------------------------------------------------------
50861ba5d63f02.....ade3a55d9b930     0        1000000000 lovelace + TxOutDatumNone

```

[^1]: The '...' within the transaction hash `TxHash` is for shortener purposes.

### 2. Build a transaction
In order to simplify next commands we will create some environment variables 

The first variable will store the transaction hash (`TxHash`) of the first Address, notice the format is <TxHash>#<TxIx>   [[Multi-witness transactions - mai 13 2022#^dd57bc|from this response]]


```bash
export UTXO01=a4f6fe6d3cd39ee15f077d6dfac88052f293a7a0385505f0d1b2b28769c75602#0
```
then do the same with thew UTXO from the second Address


javingka@javingka-B450-AORUS-ELITE:~/cardano-src/keys/test05$ export ROBERTO_ADD=addr_test1qq5wgtqtjfmr4hmnxn3k5wwkdfwm8qf2t9mxma7xzs86uyr8w89hqrqk8rw56693xzapg0f2fqs2kdph20xgm79a3cqs820z0f

build a transaction with 2 utxo
```bash
cardano-cli transaction build \
--tx-in $UTXO01 \
--tx-in $UTXO02 \
--change-address $ROBERTO_ADD \
--witness-override 2 \
--out-file tx_multi_UTXO.unsigned \
$TESTNET
```

build witness keys
```bash
cardano-cli transaction witness \
--signing-key-file paymentAdd1.skey \
--tx-body-file tx_multi_UTXO.unsigned \
--out-file paymentAdd1.witness
```

```bash
cardano-cli transaction witness \
--signing-key-file paymentAdd2.skey \
--tx-body-file tx_multi_UTXO.unsigned \
--out-file paymentAdd2.witness
```

assemble transaction

```bash
cardano-cli transaction assemble \
--tx-body-file tx_multi_UTXO.unsigned \
--witness-file paymentAdd1.witness \
--witness-file paymentAdd2.witness \
--out-file tx_multi_UTXO.signed
```

```bash
cardano-cli transaction submit \
--tx-file tx_multi_UTXO.signed \
$TESTNET
```

