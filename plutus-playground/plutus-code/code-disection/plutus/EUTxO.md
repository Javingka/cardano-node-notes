# Extended Unspent Transaction Output (EUTxO)

Extends [[UTxO]] model by adding [[script address|Script Address]] that can run arbitrary logic

When a transaction wants to consume a UTxO sitting on a [[script address|Script Address]] is validated by a node, the will run the script and then, depending the result of the script decide if the transaction is valid or not

Such script consider the existance of three infromations:

    1. Instead of just having signatures on transactions (like UTxO) we have [[redemmer|Redeemers]] 
    2. On the UTxO output side we have an additional arbitrary piece of data called Datum.
    3. Information about the transaction being validated, [[scriptContext]]

For all those three kind of information the haskell concrete datatype used is the same, at least in its low-level implementation