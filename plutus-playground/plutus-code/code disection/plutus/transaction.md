# Transaction
A transaction in Cardano is something that receives an arbitrry number of Unspent Transaction Outputs (UTxO) and deliver a specified number of utcos.

A rule of thumb with Transactions is that the amount of inputs values has to be always equals to the amounts of output values. 

For the saque of keeping this rule as true, consider the following special cases:

1. Mint new crrency: The case the Transaction mint new currency values, those values created can be considered as a  "special" kind of "input". So eventhough the total "normal' inputs will sum less than the total outputs, the minted currency can be considered as special input and as so sum to the exsting inputs
2. Burn existing currency: The case the Transaction....
3. Transactions Fee