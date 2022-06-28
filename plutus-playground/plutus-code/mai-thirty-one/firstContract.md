# Code disection


```
{-# LANGUAGE DataKinds           #-}  --Enable datatype promotions
{-# LANGUAGE FlexibleContexts    #-}  --Enable flexible contexts. Implied by ImplicitParams
{-# LANGUAGE NoImplicitPrelude   #-}  --Don't load native prelude to avoid conflict with PlutusTx.Prelude
{-# LANGUAGE ScopedTypeVariables #-}  --Enable lexical scoping of type variables explicit introduced with forall
{-# LANGUAGE TemplateHaskell     #-}  --Enable Template Haskell splice and quotation syntax
{-# LANGUAGE TypeApplications    #-}  --Allow the use of type application syntax https://gitlab.haskell.org/ghc/ghc/-/wikis/type-application
{-# LANGUAGE TypeFamilies        #-}  --Allow use and definition of indexed type and data families
{-# LANGUAGE TypeOperators       #-}  --Allow the use and definition of types with operator names
```

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


## --THE ON-CHAIN CODE
```haskell
--THE ON-CHAIN CODE
```
The following code is supposed to be stored into the blockchain.

[[alwaysSucceeds|Always Succeeds]]
```haskell
{-# INLINABLE alwaysSucceeds #-} 
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> () 
alwaysSucceeds _ _ _ = () 
```

[[validator|Validator]]
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  
```

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  
```

a validator hash deliver a key to open the "safe", or to trigger of a transaction

```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

```haskell
{-# INLINABLE alwaysFails #-}
alwaysFails :: BuiltinData -> BuiltinData -> BuiltinData -> ()   
alwaysFails _ _ _ = error () 
```

## --THE OFFCHAIN CODE
```haskell
--THE OFFCHAIN CODE
```
The following code is supposed to be stored into nodes.

```haskell
type GiftSchema =
            Endpoint "give" Integer  
        .\/ Endpoint "grab" ()
```
GiftSchema is a type with two possible Endpoint contructors. "give" whose unique parameter is of Integer type, and "grab" who expect nothing.
() <- is the Unity, kind of a void.


### give
```haskell
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount      
    ledgerTx <- submitTx tx                                                                          
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    logInfo @String $ printf "made a gift of %d lovelace" amount                                     
```
give is a function that takes an integer number, representing the amount of lovalece to transact if and inly if the defined contraints are met.

give is what trigger the transaction of giving some value. 
	what? lovelace. 
	to whom? to anyone
	
`amount` is the amount of a currency, lovelaces.

[mustPayToOtherScript](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/src/Ledger.Constraints.TxConstraints.html#:~:text=key%20address.%0A%20%20%20%20%7C-,MustPayToOtherScript,-ValidatorHash%20Datum%20Value)
The transaction must create a transaction output with a script address
```haskell
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount      
```

`mustPayToOtherScript` 
takes 

1. The hash of the recipient script (the validator function) `valHash`.
2. A Datum, in this case the datum parameter value is dummy `(Datum $ Builtins.mkI 0)`
    `Datum` constructor is used to turn Data, in this case the number 0, into a `Datum`. The `Data` is created using `Builtins.mkI` function
3. A cryptocurrency value, it seems this [value](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:Value) is a dictionary, the key is the currency symbol and the value an amount of such currency
    In this case the currency symbol is lovelace and the `amount` is an incoming parameter

and delivers

1. A transaction output. a TxConstraints [](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-constraints/html/src/Ledger.Constraints.TxConstraints.html#TxConstraint)
    "Constraints on transactions that want to spend script outputs"

`ledgerTx <- submitTx tx`
submit the transaction to the network, remember we are OFFCHAIN, well that is why we need to submit it to go ONCHAIN 
`submitTx` returns a Cardano Tx,I assume is a Cardano transaction

`getCardanoTxId` 
    takes the transaction function stored before to be sent to the network
    returns a [Tx Id](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-TxId.html), an identification of a transaction using a SHA256 hash

`awaitTxConfirmed`
    Wait until a transaction is confirmed (added to the ledger). If the transaction is never added to the ledger then awaitTxConfirmed never returns
    Returns a Contract

`logInfo @String $ printf "made a gift of %d lovelace" amount`
    If the "made a gift..." string is printed, the specified amount was actually sent into a transaction and submited to the network.


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

Notice how we start calling information about unspended transactions of a `Ledger.Addres` here with the name of `scrAddress`. Those transactions are scripts, coded to perform a validation, in this specific example the validation will always pass.

`let orefs   = fst <$> Map.toList utxos` iterate over the UTXOs getting the values (rememeber the value are maps or dictionaries with currency symbol as a key and the amount of that currency as the value) of each utxo. so `orefs` is a list of mapped currencies ammounts ready to be expended.

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