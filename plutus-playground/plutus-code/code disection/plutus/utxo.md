# UTXO Model

UTXO stands for **unspent  transaction output**

In this model a transaction has inputs and outputs. 

Inputs are unspent outputs from other previous transactions.

Assets on the ledger are stored in unspent outputs

The unspent output is specified by an address (public key hash) and a value,
such address specify what transactions can unlock the value (output) and used as an input

Unspent outputs are immutable

Is kind of weird, and have me taking some time to get used to it. Is the fact that the "unspent output", some times is referenced just as "output", other is pointed to it using the "address" that specify it. 

UTXOs are owned by users, and a balance of the Assets of a User is the sum of all its assets within all its utxos


ref: https://docs.cardano.org/plutus/eutxo-explainer