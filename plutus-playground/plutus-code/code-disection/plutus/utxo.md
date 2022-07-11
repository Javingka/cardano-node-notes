# UTXO Model

UTXO stands for **unspent  transaction output**

In this model a transaction has inputs and outputs. 

Inputs are unspent transaction outputs (UTxOs) from other previous transactions.

Assets on the ledger are stored in unspent outputs

The unspent transaction output (UTxO) is specified by an address (such aaddress is given by the hash of the public key) and a value,
such address specify what transactions can unlock the value (output) and used as an input

Unspent transactions outputs are immutable

Is kind of weird, and have me taking some time to get used to it. Is the fact that the "unspent transaction output", some times is referenced as "unspent output" or just as "output", other is pointed to it using the "address" that specify it. Could be a little  bit silly but from a learning experience point of view is something to consider for anyone trying to explain and teach about Cardano Ecosystem. 

UTXOs are owned by users, and a balance of the Assets of a User is the sum of all its assets within all its utxos


ref: https://docs.cardano.org/plutus/eutxo-explainer