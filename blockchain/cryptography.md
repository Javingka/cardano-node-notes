There are three kind of cryptografies primitives.

1. unkeyed
	1. hash functions
	2. Pseudo random numbers
2. symmetric keys
	1. Block ciphers
	2. Stream ciphers
3. asymmetric keys
	1. Public key ciphers
	2. digital signatures

## Asymmetric Keys
Asymmetric Key cryptography uses a pair of keys, public / private pairs.
Public keys are generated using the private key

## Creating Digital Signature
1. The Signer or Sender has **data** want to share
2. The Signer or Sender needs to create a **hash value** for the data.
3. The Signer or Sender needs to **encrypt** such hash ussing her **private Key**
4. The Signer or Sender attach the following into a message
	- Encrypted hash value. <- AKA the **Signature**
	- Sender public key. <- In cardano the public key is the address (hash) of the Sender
	- data that wants to share.
	- Certificate <- this part is not clear for me. What is the certificate? why is necessary? 
5. The Signer or Sender sends  the message to the Receiver

6. Receiber reicives the message. A **Signature** (encrypted hash) from an Address (**public** key) next with a Document (Data )
7. The receiver verifies:
	1. Separate the:
		1. **Signature**
		2. Data
	2. Using the public key (Address of the sender) decrypt the Signature (encrypted hash). So get the hash of the data created by the Sender
	3. The Receiver also creates a **hash value** of the received data
	4. The Receiver compares both the hash values:
		- the one recently created (previous step).The one the receiver created 
		- the one got from the decryption. The one the sender created.
	5. Are the same? the message has no changed and so the signature is Valid
	6. Are Different? the message has changed or the signature is invalid

![[Captura de Tela 2022-07-03 às 23.36.33.png]]