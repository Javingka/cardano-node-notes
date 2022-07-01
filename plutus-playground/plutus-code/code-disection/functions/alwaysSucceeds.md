```haskell
{-# INLINABLE alwaysSucceeds #-} 
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> () 
alwaysSucceeds _ _ _ = ()
```

The function is defined as INLINABLE to make possible to rely in other functions without need to write those helper functions within haskell template code

The most easy way to read the function is to say the function recebes three "any" parameters and deliver a unit

I don't exactly know, but I would expect to this function return a True insted a unit.

But, looking a little bit more on [[firstContract|.hs code]] this `alwaysSucceds` function will be compiled into a function to deliver a verificator that in turn will be used to generate a validator hash, making use of the result as a key to open the "safe", a trigger of a transaction



