```haskell
{-# INLINABLE alwaysSucceeds #-} 
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> () 
alwaysSucceeds _ _ _ = ()
```

The function is defined as INLINABLE to make possible to rely in other functions without need to write those helper functions within haskell template code

The most easy way to read the function is to say the function recebes three "any" parameters and deliver a unit

I don't exactly know, but I would expect to this function return a True insted a unit. 
    Consulting about this doubt, the thing is that the unit (also known as 'empty tuple') makes possible to leave the flow keep going if the varification succeeds with no special output. It's good to think on it as a "Void" function in other programming languages. Also consider that if the funtion would return a Boolean, it means that returns a value, that as a size in bytes, avoid having to store size even if it seems pretty low, at the end and in scale endsup making difference. So is Unit, becaouse is the most elegant and eficient solution.

But, looking a little bit more on [[firstContract|.hs code]] this `alwaysSucceds` function will be compiled into a function to deliver a verificator that in turn will be used to generate a validator hash, making use of the result as a key to open the "safe", a trigger of a transaction



