# Script Address


From what we learn from cardano-cli operations. A Cardano Address is a unique and identifiable 'place'.

A script address represents one of the two ways the UTXO model is extended to EUTXO model.

In UTXO model, a transaction can use a [[utxo|unspent transaction output]] as input if unlock the value using the right signature.
But, already in EUTXO model, the address specifying the UTXO can contain arbitrary logic in the form of scripts

In order to build an Address we need an address key pair:
- A verification key
- A signing key

Optionally when creating an Address can be include a staking key pairs:
- A verification key
- A signing key

With those one or two key pairs, and identifying in what network the Addres will be created the address can be built.

An address looks like this: 
`addr_test1vrz9k3jwwahpl9t6awkns89kmsajc2hfgxe3psh800mq0nq62k0sv`
is a hash