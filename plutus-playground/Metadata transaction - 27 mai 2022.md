This metadata created will be used in future session.

### 1. Create metadata file

Write a JSON file, follow this same structure. Named as `metadata.json`

```json
{
	"2022":{
		"0530": {
			"name": :"Javier",
			"comment": "was here!",
			"batch": "45"
		}
	}
}
```

### 2. Create an address 
Create an address in order to be able to send a transaction.

Follow the [[Address#4 Create an address]] notes to do so.

After create the Account. [Get fund it](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/)

### 3. Build a transaction

Build a transaction using the UTXO from the previous created Address. Refer to this [[Metadata - apr 29 2022#3 Build a transaction]] as a reference (in such note the transaction was made manually calculating the fees).

Bear in mind that the target Address is Roberto's one `addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt`

So the transaction will be like this:

1. Store the UTXO into a variable
```bash
UTXO=3efb889e11609fdccfa04b10c343f38388ceb718a8ef53f334a391147a5c0904#0
```

2. Store the destination address into a variable: 
```bash
addressDestination=addr_test1qpc6mrwu9cucrq4w6y69qchflvypq76a47ylvjvm2wph4szeq579yu2z8s4m4tn0a9g4gfce50p25afc24knsf6pj96sz35wnt
```
   
3. Create the transaction. The variable `addressSource` is the name I used for the variable that store the Addres I created before
```bash
cardano-cli transaction build \
--tx-in  $UTXO \
--tx-out $addressDestination+100000000 \
--change-address $addressSource \
--metadata-json-file metadata.json \
--out-file tbf-tx.unsigned \
$TESTNET
```


### 4. Sign the transaction
```bash
cardano-cli transaction sign \
--tx-body-file tbf-tx.unsigned \
--signing-key-file payment.skey \
$TESTNET \
--out-file tbf-tx.signed
```

### 5. Submit the transaction

```bash
cardano-cli transaction submit \
--tx-file tbf-tx.signed \
$TESTNET
```

### 6. Check the transaction 

```bash
cardani-cli query utxo \
--address $addressSource \
$TESTNET
```

Then just checkout the transaction. In my case can be found [here](https://testnet.cardanoscan.io/transaction/23a1c125da32dcf211bf785f9627ae924bf0957b4c7619293b3de0ee41c1b2c9?tab=metadata):

Transaction hash: `23a1c125da32dcf211bf785f9627ae924bf0957b4c7619293b3de0ee41c1b2c9`
Metadata Hash: 
`13a1e6099956327df9de6032bcfcc9fd72dbcb95967c19c635d36105a48fa8ca`


