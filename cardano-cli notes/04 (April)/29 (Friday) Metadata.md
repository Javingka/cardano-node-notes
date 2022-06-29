Useful references:
[input-output-hk/cardano-node/transaction metadata](https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md)
[How to create a metadata transaction using cardano-cli](https://developers.cardano.org/docs/transaction-metadata/how-to-create-a-metadata-transaction-cli/)

### 1. Build 2 Addresses

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

### 2. Build metadata object

Create a file called `metadata.json`  with the following content

```json
{
	"2020": {
		"0730": {
			"batch": 45,
			"user": "UserName",
			"dummy": 1,
			"session": "09"
		}
	}
}
```


### 3. Build a transaction

First let read the transaction hash + transaction index used as input of the transaction.

1. query the UTOXs of the address1
```bash
   cardano-cli query utxo --address $(cat paymentAddress1.addr) $TESTNET
```
response:
```bash
    TxHash                          TxIx        Amount
-----------------------------------------------------------------------------------
1d345f3af4f63df....23dbcb63f4cc65     0        1000000000 lovelace + TxOutDatumNone

```

2. store the transaction hask + transaction index into a Environment variable
```bash
export UTXO=1d345f3af4f63df....23dbcb63f4cc65#0
```

3. Build the transaction draft. We will need to calculate fee later.

```bash
cardano-cli transaction build-raw \
--tx-in $UTXO \
--tx-out $(cat paymentAddress2.addr)+1000000 \
--tx-out $(cat paymentAddress1.addr)+0 \
--metadata-json-file metadata.json \
--fee 0 \
--out-file meta_tx.draft
```
In this transaction we are sending 1 ADA from Address1 to Address2

4. Calculate the fee

In order to get the fee calculated the protocol json file should exist, if not, create it calling the flollowing command:

```bash
cardano-cli query protocol-parameters \
--out-file protocol.json \
$TESTNET
```

Now, it's ok to actually calculate the fee

```bash
cardano-cli transaction calculate-min-fee \
--tx-body-file meta_tx.draft \
--tx-in-count 1 \
--tx-out-count 2 \
--witness-count 1 \
--protocol-params-file protocol.json \
$TESTNET 
``` 

The response of the previous command should look like this:
```bash
180857 Lovelace
```

5. Do the math to get the values

```JS
input_from_add1 = 1.000.000.000
output_to_add2 = 1.000.000
fee = 180857
output_to_add1 = input_from_add1 - output_to_add2 - fee // 998819143

```
`output_to_add1` is the change of the transaction

6. write the unsigned transaction with the calculated values

```bash
cardano-cli transaction build-raw \
--tx-in $UTXO \
--tx-out $(cat paymentAddress2.addr)+1000000 \
--tx-out $(cat paymentAddress1.addr)+998819143 \
--metadata-json-file metadata.json \
--fee 180857 \
--out-file meta_tx.unsigned
```

### 4. Sign the transaction

```bash
cardano-cli transaction sign \
--tx-body-file meta_tx.unsigned \
--signing-key-file paymentAdd1.skey \
--out-file meta_tx.signed \
$TESTNET 
```

### 5. Submit the transaction
```bash
cardano-cli transaction submit \
--tx-file meta_tx.signed \
$TESTNET
```

### 6.  Verify 
Calling
```bash
cardano-cli query utxo --address $(cat paymentAddress2.addr) $TESTNET
```

you should see as response the address2's transaction hashes  with a new transaction hash, with the value sent, like so:
```bash
			TxHash                    TxIx        Amount
-----------------------------------------------------------------------------------
95e5c773f6652b8....ae7862ae52f160c     0        1000000 lovelace + TxOutDatumNone

```

copy that hash and go to [testnet cardano scan](https://testnet.cardanoscan.io/) paste the hash in the search field and check the result, as in the image when you click on metadata tab, and click again over the 'Value'  you will see something like the image:

![[ScreenshotFromCardanoscan.png]]