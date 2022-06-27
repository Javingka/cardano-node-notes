Validator

``` haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])  --2nd example change this to alwaysFails
```

This function creates a function called **validator**

The `$$(...)`  pattern seems to be the encapsulation of a  [[haskell template|haskell template]] execution.

What specific type of [[haskell template|haskell template]] is? 
	Why double brackets? 

Anyway, the haskell template is aimed to create code on compiling time. Is meta-programing, code to write code.

So what we are coding is a function created to validate an input as true or false. 
The name of the function is **alwaysSucceeds** , and is a very basic function  

```haskell
mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator
```