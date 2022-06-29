[Reference](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/building-and-signing-tx.md)

### 1. Setup Envirnment variables
```bash
export TESTNET="--testnet-magic 1097911063"
```

### 2. Grab protocol parameters
Is good to recall protocol parameter as Cardano keep changing, and store it into a file. In this case we use `protocolparams.json`

```bash
cardano-cli query protocol-parameters \
$TESTNET \
--out-file protocolparams.json
```

### 3. Build 2 Addresses

Build the addresses creating both payment and staking keys

```bash
cardano-cli address key-gen \
--verification-key-file paymentAdd1.vkey \
--signing-key-file paymentAdd1.skey
```

```bash
cardano-cli stake-address key-gen \
--verification-key-file stakeAdd1.vkey \
--signing-key-file stakeAdd1.skey
```

```bash
cardano-cli address build \
--payment-verification-key-file paymentAdd1.vkey \
--stake-verification-key-file stakeAdd1.vkey \
--out-file paymentAddress1.addr \
$TESTNET
```

Make sure you have credits on each Address, use [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) to send Ada to one of the Address. Be aware faucet can send only 1 time each 24 hours from the same IP .

to check the UTXOs availables in those Address call
```bash
cardano-cli query utxo --address $(cat paymentAddress1.addr) $TESTNET
```


### 4. Create a transaction
With the rseponse of the previous command get the transaction hash (`TxHash`)+ transaction index (`TxIx`) and stored into an envirnment vairable. Notice we use **#** to join the two infomation, like this:

``` bash
export utxo=3826ae8152a921fdc561...df7009b3d#0
```

```bash
cardano-cli transaction build-raw \
--shelley-era \
--tx-in $utxo \
--tx-out $(cat paymentAddress2.addr)+500000000 \
--tx-out $(cat paymentAddress1.addr)+0 \
--invalid-hereafter 0 \
--fee 0 \
--out-file tx.draft
```


- **Invalid-hereafter** - represents a slot, or deadline by which a transaction must be submitted. This is an absolute slot number, rather than a relative one, which means that the `--invalid-hereafter` value should be greater than the current slot number. A transaction becomes invalid at the invalid-hereafter slot.

The previous command will generate a file named `tx.draft` with the following format:

```json
    "type": "TxUnsignedShelley",
    "description": "",
    "cborHex": "83a400818258203826ae8152a921fdc561d54b28dc1944fef07c442eecde3d30aae59df7009b3d000182825839002055fca460e6cf51b2a196cc98722d2ab846cb33114a56a5318350a8950d49312fee50ee3e62a79406881c228344416c7e732324810bcc0a1a1dcd650082583900ad2d2d535899e39212c1cd2ec7890f5a29dcbba68486cad0ea87d79a368684126f5e74028f62f9a2dfc2e9943cc98920cca0f4cd09b59bd400020003009ffff6"
}

```

here `cborHex` is the variable storing all the information about the transaction in `cbor` format. that is what effectively is stored into the block chain.

Notice is not yet a finished transaction. We still need to setup the actual values and calculate the fees

To read in human readably format run the following command
```bash
cardano-cli transaction view --tx-body-file tx.draft
```

To  decode the transaction draft as cbor code, run the following code:
```bash
cardano-cli text-view decode-cbor --in-file tx.draft --out-file tx.draft2
```

### 5. Calculate fee

Calculate the fee

```bash
cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 1 \
--tx-out-count 2 \
--witness-count 1 \
--protocol-params-file protocolparams.json \
$TESTNET 
```

When run this command, the response is the ammount of fee it will need to be paid in lovelace

`176897 Lovelace`  A very common value in this case.

Do the transaction math 
input             -  output           -  fee       =  change
100,000,000 - 500,000,000 - 176,897 = 499,823,103

calculate at what slot
58265117 <- actual
58266117 <- the new

### 6.  Build actual transaction
```bash
cardano-cli transaction build-raw \
--shelley-era \
--tx-in $utxo \
--tx-out $(cat paymentAddress2.addr)+500000000 \
--tx-out $(cat paymentAddress1.addr)+499823103 \
--invalid-hereafter 58266117 \
--fee 176897 \
--out-file tx.raw
```

To define what slot number we will use to setup the `--invalid-hereafter`  parameter, we call the following command

```bash
cardano-cli query tip $TESTNET
```

### 7. Sign and submit transaction
```bash
cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file paymentAdd1.skey \
--out-file tx.signed \
$TESTNET
```

```bash
cardano-cli transaction submit \
--tx-file tx.signed \
$TESTNET
```

Be aware the Slot number used into `--invalid-hereafter` parameter has to be calculated correctly so by the time the transaction is submitted, the Slot numbar still does not happen. 



