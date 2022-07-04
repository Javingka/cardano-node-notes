Validator

``` haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  --2nd example change this to alwaysFails
```

This function creates a function called **validator**

The `$$(...)`  pattern seems to be the encapsulation of a  [[haskell template|haskell template]] execution.

What specific type of [[haskell template|haskell template]] is? 
	Why double brackets? because is the format of Oxford Brackets, 

Anyway, the haskell template is aimed to create code on compiling time. Is meta-programing, code to write code.

So what we are coding is a function created to validate an input as true or false. <- is this assertion true?

The name of the function is **alwaysSucceeds** , and is a very basic function  

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