# Transaction
A transaction in Cardano is something that receives an arbitrry number of Unspent Transaction Outputs (UTxO) and deliver a specified number of UTxOs.

A rule of thumb with Transactions is that the amount of inputs values has to be always equals to the amounts of output values. 

For the saque of keeping this rule as true, consider the following special cases:

1. Mint new currency: 
	- Input are lower than outputs.
	- The transaction mint new currency values, those values created can be considered as a  "special" kind of "input". So eventhough the total "normal' inputs will sum less than the total outputs, the minted currency can be considered as special input and as so sum to the existing inputs, keeping the balance.
2. Burn existing currency
	- Input are higher than outputs.
	- The transaction burn existing currency values, those are inputs of a transaction that send such values to a special address where the output is un-spendable.
3. Transactions Fee
	- All transactions in the blockchain must pay a fee, this fee is calculated regarding the amount of information involved. This fee should be sum to the total of delivederd UTxOs of the transaction