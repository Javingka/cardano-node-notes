# Code disection

This file disects the content of the [**firstContract.hs**](../haskell/firstContract.hs.md) file as an excercise for understanding the code. 

Mind in Obisidan files other than ".md" won't be visible. So the code referenced before include the '.md' at the end.

This documents contains my understanding on the matter and is supposed to get updated as well as my understanding.
So probably contains some wrong interpretations.

---
## File header 
The code starts with a series of "pragma" statements to specify some extra-linguistic information to GHC [ref](https://stackoverflow.com/questions/22773699/purpose-of-in-haskell) 

```haskell
{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax https://gitlab.haskell.org/ghc/ghc/-/wikis/type-application
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names
```

---
## Module declaration and imports

Next, the name of the Haskell module and a series of library imports used in the code
```haskell
module OurGift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
```

---

## On Chain code

The following code is supposed to be stored into the blockchain. So it is called "on chain" 

```haskell
--THE ON-CHAIN CODE
```

---
<div style="text-align: right">  </div>
### alwaysSucced
[[alwaysSucceeds|Always Succeeds]]
This function will be used to generate a Verificator function aimed to execute a transaction validation (in this case, as the name declare, it will always succeeds)
```haskell
{-# INLINABLE alwaysSucceeds #-} 
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> () 
alwaysSucceeds _ _ _ = () 
```

---

### Validator

[[validator|Validator]]
This function can be considered as a variable holding the script that should be run on the blockchain.
The script is wrapet into the `Validator` type.
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  
```

---

### valHash
```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  
```

A validator hash is used as a key to open a "safe", to be more accurate is a key used to trigger a transaction

```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Address with two kinds of Credentials, normal and staking. [Required to unlock a transaction output](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Credential.html#t:Credential)
- Credential has two constructors:
    - PubKeyCredential PubKeyHash        
        <br>The transaction that spends this output must be signed by the private key
    - ScriptCredential ValidatorHash        
        <br>The transaction that spends this output must include the validator script and be accepted by the validator


---

### alwaysFails
Use this function to replace alwaysSucceds in order to make the Contract always fails.

```haskell
{-# INLINABLE alwaysFails #-}
alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()   
alwaysFails _ _ _ = error () 
```

---

##  Off chain code

The following code is supposed to be stored into nodes.

```haskell
--THE OFFCHAIN CODE
```

### GiftSchema
GiftSchema is a type with two possible contructors (two Endpoints):

- `"give"` whose unique parameter is of Integer type.
- `"grab"` who expect nothing. `()` <- is the Unity, kind of a void.

```haskell
type GiftSchema =
            Endpoint "give" Integer  
        .\/ Endpoint "grab" ()
```

---

### give
[[give]] is the function that submits a transaction between an Address giving some lovelace to any other Address who ask for it, for free!

```haskell
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount      
    ledgerTx <- submitTx tx                                                                          
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    logInfo @String $ printf "made a gift of %d lovelace" amount
```

give takes an `Integer` number representing the `amount` of lovalece to give if the validation is pass (in this case always pass, that is why we call a gift). This function then compile the validation Script into a Transaction Output and submit it to the network.

--- 

### grab
```haskell
grab :: forall w s e. AsContractError e => Contract w s e ()                                     
grab = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                     -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "collected gifts"                                                              -- Log information 
```

Notice how we start calling information about unspended transactions of an Addrees (of type `Ledger.Addres` ) here with the name of `scrAddress`. 

Those transactions are scripts, coded to perform a validation, in this specific example the validation will always pass.

`let orefs   = fst <$> Map.toList utxos` iterate over the UTXOs getting the values (rememeber the value are maps or dictionaries with currency symbol as a key and the amount of that currency as the value) of each utxo. so `orefs` is a list of mapped currencies ammounts ready to be expended.

---

### endpoints
```haskell
endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" $ const grab                                                            -- block until grab
```

```haskell
mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies []                                                                                 -- MakeKnown currencies for the playground to have some ADA available
```