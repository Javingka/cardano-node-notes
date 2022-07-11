Validator

``` haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  --2nd example change this to alwaysFails
```

This Haskell function works as a variable that store a function called **validator** wich ultimately is a script that will live at the blockchain in PlutusCore

The `$$(...)`  pattern seems to be the encapsulation of a  [[haskell template|haskell template]] execution. 
  We are asking the compiler to write the code for the validator function at compile time based on our mkValidator function, and then proceed with the normal compilation

What specific type of [[haskell template|haskell template]] is? 
	Why double brackets? because is the format of Oxford Brackets, 

Anyway, the haskell template is aimed to create code on compiling time. Is meta-programing, code to write code.

So what we are coding is a function created to validate an input, in the case it do validates do nothing (hence the () datatype) otherwise through an error accordingly

The name of the function is **mkValidatorScript** , and is a very basic function  

```haskell
mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
```

A [Validator](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:Validator) is a wrapper around Scripts, wich are used as validators in transaction outputs

The function definition is as such:
```haskell
-- | 'Validator' is a wrapper around 'Script's which are used as validators in transaction outputs.
newtype Validator = Validator { getValidator :: Script }
  deriving stock (Generic)
  deriving newtype (Haskell.Eq, Haskell.Ord, Serialise)
  deriving anyclass (ToJSON, FromJSON, NFData)
  deriving Pretty via (PrettyShow Validator)
```

[Script](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html#t:Script) 
Is a script on the chain, Is an opaque type (exposes its definition but hides type constructors) 