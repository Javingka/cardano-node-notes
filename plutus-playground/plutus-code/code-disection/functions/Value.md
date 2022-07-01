[Value](https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Value.html#t:Value)
Is the value of a specific token on a specific UTXO

Constructor: 
```haskell
Value	 
    getValue :: Map CurrencySymbol (Map TokenName Integer)	 
```

**CurrencySymbol** is actually is the **PolicyId** 

What is the currency symbol of Ada? Nothing! and what is the TokenName? also Nothing! because is the default in Cardano