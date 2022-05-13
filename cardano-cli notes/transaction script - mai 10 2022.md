mai 10 2022

Several commands used here will call a Environment Variable named `$TESTNET`. it's essential such variable exists on ~/.bashrc file as follows:
```bash
export TESTNET="--testnet-magic 1097911063"
```

### 1. Setup 4 addresses
First we created 4 different addresses, 
For each address we run the following commands:

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
--out-file Address1.addr \
$TESTNET
```

Yes, the previous commands should be executed 4 times each, changing the names

Notice the examples above are wrote defining `paymentAdd1` and `stakeAdd1` as the names for the verification (`.vkey` ) and signing (`.skey`) keys. And `Address1.addr` as the name of the file holding the Address hash. 

To check the UTXOs of any address, use the following command. The  `$(cat Address4.addr)` is used to read the hash from the **Address4.addr** file and used as parameter value
```bash
cardano-cli query utxo \
--address $(cat Address4.addr) \
$TESTNET
```

All previous names are arbitrary, meaning you can use wherever name you want.

### 2. Key Hashes generation
Then we create a key-hash for **3** of the 4 recently created addresses:

```bash
cardano-cli address key-hash \
--payment-verification-key-file paymentAdd1.vkey \
--out-file KEY_HASH_ADD_1.hkey
```

```bash
cardano-cli address key-hash \
--payment-verification-key-file paymentAdd2.vkey \
--out-file KEY_HASH_ADD_2.hkey
```

```bash
cardano-cli address key-hash \
--payment-verification-key-file paymentAdd3.vkey \
--out-file KEY_HASH_ADD_3.hkey
```

### 3. Create a script file
Next we created a script file with the following text, we will name the file as `multisignpolicy.script`.

Here we use the hashes created on previous step, meaning you have to use the hash values stored in `KEY_HASH_ADD_1.hkey`, `KEY_HASH_ADD_2.hkey` and `KEY_HASH_ADD_3.hkey` files

```json
{
"type":"all",
"scripts":
        [
			{
				"type":"sig",
				"keyHash":"<KEY_HASH_ADD_1.hkey>"
			},
			{
				"type":"sig",
				"keyHash":"<KEY_HASH_ADD_2.hkey>"
			},
			{
				"type":"sig",
				"keyHash":"<KEY_HASH_ADD_3.hkey>"
			}
        ]
}
```
**Mind the format of the JSON file.  I.e. use of capital letters,  ':',  ', ' etc**

In the code above the `<KEY_HASH_ADD_1.hkey>`  strings should be replaced with the actual hash found in the files of the same name.

### 4. Create an Account address for the script
Then, we will create an account from the script.

```bash
cardano-cli address build \
--payment-script-file multisignpolicy.script \
--out-file multisignaddress.addr \
$TESTNET
```

We use `multisignaddress.addr` as the name of the file storing the address hash

### 5. Credit the address
Go to faucet and send credits to the new multisignaddress Address https://testnets.cardano.org/en/testnets/cardano/tools/faucet/

### 6. Setup a transaction
We will send a utxo to Roberto addresss account: `addr_test1qp5h55qm05zhv6eapr3em26rurchp9eaxwvt9p69kwuvu42rj9ynfzwggc0s55nlpqegv90w2rshnqf0q3um5pytk4qqutq8j6`  
( but this could also be sent to the 4th account created before)

Build the transaction:
```bash
cardano-cli transaction build \
--tx-in da5dcaa117e27f324430a5fe2fc32f86cf7ab03f6556800b84e977a6a526b765#0 \
--change-address addr_test1qp5h55qm05zhv6eapr3em26rurchp9eaxwvt9p69kwuvu42rj9ynfzwggc0s55nlpqegv90w2rshnqf0q3um5pytk4qqutq8j6 \
--tx-in-script-file multisignpolicy.script \
--witness-override 3 \
--out-file txmultisign.raw \
$TESTNET
```
For `--tx-in` parameter we use the transaction hash from the return of `cardano-cli query utxo --address $(cat multisignaddress.addr) $TESTNET` command. 
In **my** case is: `da5dcaa117e27f324430a5fe2fc32f86cf7ab03f6556800b84e977a6a526b765` 

For `--change-address` parameter, as commented before, we will use Roberto's address.

The transaction raw information will be stored into the file `txmultisign.raw` 

Once executed the command we can view transaction draft calling the following:
```bash
cardano-cli transaction view \
--tx-body-file txmultisign.raw
```
Response should be something like this:
```bash
auxiliary scripts: null
certificates: null
collateral inputs: []
era: Alonzo
fee: 178657 Lovelace
inputs:
- da5dcaa117e27f324430a5fe2fc32f86cf7ab03f6556800b84e977a6a526b765#0
metadata: null
mint: null
outputs:
- address: addr_test1qp5h55qm05zhv6eapr3em26rurchp9eaxwvt9p69kwuvu42rj9ynfzwggc0s55nlpqegv90w2rshnqf0q3um5pytk4qqutq8j6
  address era: Shelley
  amount:
    lovelace: 999821343
  datum: null
  network: Testnet
  payment credential:
    key hash: 697a501b7d05766b3d08e39dab43e0f170973d3398b28745b3b8ce55
  stake reference:
    key hash: 4391493489c8461f0a527f08328615ee50e179812f0479ba048bb540
update proposal: null
validity range:
  lower bound: null
  upper bound: null
withdrawals: null
```

### 7. Witnesses sign the transaction
Then each of the 3 withnesses sign the transaction

```bash
cardano-cli transaction witness \
--signing-key-file paymentAdd1.skey \
--tx-body-file txmultisign.raw \
--out-file paymentAdd1.witness
```

```bash
cardano-cli transaction witness \
--signing-key-file paymentAdd2.skey \
--tx-body-file txmultisign.raw \
--out-file paymentAdd2.witness
```

```bash
cardano-cli transaction witness \
--signing-key-file paymentAdd3.skey \
--tx-body-file txmultisign.raw \
--out-file paymentAdd3.witness
```

Notice the `--signing-key-file` and `--out-file` parameters values change on each command. 

### 8. Assamble the transaction
Now, assemble the transaction.

Roberto gave an example explaining why is it an assamble:
>Imagine you have a paper document with 3 sign spaces, and then you send to each guy kind of the same thing, a 'contract'. 
>
>They will take the piece of the contract where their signature goes and they gona take that part and put their signature. 
>
>Then you will take those three signed parts and assembled it into the final contract, like a puzzle.
>


```bash
cardano-cli transaction assemble \
--tx-body-file txmultisign.raw \
--witness-file paymentAdd1.witness \
--witness-file paymentAdd2.witness \
--witness-file paymentAdd3.witness \
--out-file txmultisign.signed
```

### 9.Submit the transaction
```bash
cardano-cli transaction submit \
--tx-file txmultisign.signed \
$TESTNET
```

Review the founds of the transaction address, it should be empty
```bash
cardano-cli query utxo --address $(cat multisignaddress.addr) $TESTNET
```

