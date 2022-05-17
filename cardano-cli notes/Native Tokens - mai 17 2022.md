# Native Tokens
[ref](https://developers.cardano.org/docs/native-tokens/minting/)

### 1. Setup some variables

#### 1.1  Names of the token to mint

**Since cardano-cli version 1.31.0, token names must be base16 encoded** . So here, we use the xxd tool to encode the token names.

```bash
tokenname=$(echo -n "Bash45Token" | xxd -ps | tr -d '\n')
```

#### 1.2 Get Protocol parameters
```bash
cardano-cli query protocol-parameters --out-file protocol.params $TESTNET
```

Store the protocol parameters in the folder you are working on.

### 2. Build 1 Address

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

Write a temporal environment variable to store the address
```bash
Address1=$(cat paymentAddress1.addr)
```
Make sure you have credits on the address, use [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) to do so. Be aware faucet can send only 1 time each 24 hours from the same IP addreess.

### 4. Generate the policy

Policies are the defining factor under which tokens can be minted. Only those in possession of the policy keys can mint or burn tokens minted under this specific policy.

We'll make a separate sub-directory in our work directory to keep everything policy-wise separated and more organized

```bash
mkdir policy
```

#### 4.1 Create policy key pairs

```bash
cardano-cli address key-gen \
--verification-key-file policy/policy.vkey \
--signing-key-file policy/policy.skey
```


#### 4.2 Create key-hash for the policy address

```bash
cardano-cli address key-hash \
--payment-verification-key-file policy/policy.vkey \
--out-file policy/policy.kHash
```

#### 4.3 create policy script
Create the file and fill it with empty string

```Bash
touch policy/policy.script && echo "" > policy/policy.script
```

and populate the file
```bash
echo "{" >> policy/policy.script 
echo "  \"keyHash\": \"$(cat policy/policy.kHash)\"," >> policy/policy.script 
echo "  \"type\": \"sig\"" >> policy/policy.script 
echo "}" >> policy/policy.script

```

The previous command will generate the following content for the file, `<policy_key_hash>` should be replaced by the actual key hash generated on step 4.2

```json
{
  "keyHash": "<policy/policy.kHash>",
  "type": "sig"
}
```

#### 4.4 Generate policy ID

```bash
cardano-cli transaction policyid \
--script-file ./policy/policy.script > policy/policyID
```

The `> policy/policyID` part of the previous command do store the actual policy ID into a file named `policyID` within `/policy` folder

### 5. Build transaction 

Important to notice in this example the input and output of the transaction will be the same - **our own Address1**


#### 5.1  Build raw transaction

First, we will build a transaction, resulting in a file. This will be the foundation of how the transaction fee will be calculated.

##### 5.1.1 Prepare parameters values
Start by query your payment address (Address1) and take note of the different values present

```bash
cardano-cli query utxo --address $Address1 $TESTNET
```

response should be like this: [^1]
```bash
        TxHash                  TxIx    Amount
--------------------------------------------------------------------------
789bf79422aaaf.....f72831e2f     0   1000000000 lovelace + TxOutDatumNone
```
[^1]: The '...' within the transaction hash `TxHash` is for shortener purposes.

Since we need each of those values in our transaction, we will store them individually in a corresponding variable.

```bash
txhash="insert your txhash here"
txix="insert your TxIx here"
funds="insert Amount here"
policyid=$(cat policy/policyID)
```

In my case:
```bash
txhash="789bf79422aaafff29b7247a613857bde309d3b34db9d4fe34ac090f72831e2f"
txix="0"
funds="1000000000"
policyid=$(cat policy/policyID)
```

##### 5.1.2 build raw transaction

```bash
cardano-cli transaction build-raw \
--fee 0 \
--tx-in $txhash#$txix \
--tx-out $Address1+0+"10000000 $policyid.$tokenname" \
--mint="10000000 $policyid.$tokenname" \
--minting-script-file policy/policy.script \
--out-file batch45ntTX.raw
```

#### 5.2 Calculate the fees

Based on this `batch45ntTX.raw` transaction we can calculate the minimal transaction fee 

```bash
cardano-cli transaction calculate-min-fee \
--tx-body-file batch45ntTX.raw \
--tx-in-count 1 \
--tx-out-count 1 \
--witness-count 2 \
$TESTNET \
--protocol-params-file protocol.params | cut -d " " -f1
```

The same fee calculation as above but storing it in the variable `$fee`

```bash
fee=$(cardano-cli transaction calculate-min-fee --tx-body-file batch45ntTX.raw --tx-in-count 1 --tx-out-count 1 --witness-count 2 $TESTNET --protocol-params-file protocol.params | cut -d " " -f1)

```

 To calculate the remaining output we need to subtract the fee from our funds and save the result in our output variable.

```
output=$(expr $funds - $fee)
```

#### 5.3 build unsigned transaction
Then we need to re-build the transaction, including the correct fee and the adjusted amount we're able to send. Since we send it to ourselves, the output needs to be the amount of our fundings minus the calculated fee.

```bash
cardano-cli transaction build-raw \
--fee $fee \
--tx-in $txhash#$txix \
--tx-out $Address1+$output+"10000000 $policyid.$tokenname" \
--mint="10000000 $policyid.$tokenname" \
--minting-script-file policy/policy.script \
--out-file batch45ntTX.unsigned
```

### 6. Sign and submit transaction

```bash
cardano-cli transaction sign  \
--signing-key-file paymentAdd1.skey \
--signing-key-file policy/policy.skey  \
--tx-body-file batch45ntTX.unsigned  \
$TESTNET \
--out-file batch45ntTX.signed
```

```bash
cardano-cli transaction submit --tx-file batch45ntTX.signed $TESTNET
```

After a couple of seconds, we can check the output address

```bash
cardano-cli query utxo --address $Address1 $TESTNET
```
