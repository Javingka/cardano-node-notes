# give

This function refers to [[firstContract#give]] code

give is what submit a transaction of giving some lovelace to anyone who ask, for free!
```haskell
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount      
    ledgerTx <- submitTx tx                                                                          
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                               
    logInfo @String $ printf "made a gift of %d lovelace" amount
```
give is a function that takes an integer number, representing the amount of lovalece to give (transact) if the validation is pass (in this case always pass that is why we call a gift). This function then compile the validation Script into a Transaction Output and submit it to the network.

	
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
