# First Redeemer
## Code disection

This file disects the content of the [**firstRedeemer.hs**](../haskell/firstRedeemer.hs.md) file as an excercise for understanding the code. 

Mind in Obisidan files other than ".md" won't be visible. So the code referenced before include the '.md' at the end.

This documents contains my understanding on the matter and is supposed to get updated as well as my understanding.
So probably contains some wrong interpretations.

---
## File header 
The code starts with a series of "pragma" statements to specify some extra-linguistic information to GHC [ref](https://stackoverflow.com/questions/22773699/purpose-of-in-haskell) 

```haskell
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
```
---
## Module declaration and imports

Next, the name of the Haskell module and a series of library imports used in the code
```haskell
module OurGoodRedeemer where

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

### goodRedeemer
This function will be used to generate a Verificator function aimed to execute a transaction validation. It will pass only if the second parameter (the redeemer) receives the Integer number 42 
```haskell
{-# INLINABLE goodRedeemer #-} 
goodRedeemer :: BuiltinData -> BuiltinData -> BuiltinData -> ()
goodRedeemer _ redeemer _ 
 | redeemer == Builtins.mkI 42 = ()
 | otherwise                   = traceError "Wrong Redeemer"

```

---

### Validator

[[validator|Validator]]
This function can be considered as a variable holding the script that should be run on the blockchain.
The script is wrapet into the `Validator` type.
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| goodRedeemer ||])
```

---

### valHash
This is the hash of the validator
```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator  
```

The function to get the address of the script within the blockchain
```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

