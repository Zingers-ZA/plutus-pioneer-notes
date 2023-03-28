# UTXO Model:

- unspent transaction outputs
- an address can own a utxo
- addresses can tranfer funds by spending(consuming) utxos and generating new ones with new owners
- the signatures of owners are used to determine whether the utxo is allowed to be spent

# ETUXO model:

- a script can own a utxo
- the logic to decide whether a utxo can be spent is determined by the arbitrary logic within the script

- the `redeemer` acts as the justification to allow spending of the utxo
  - the script <b>only</b> has the redeemer to decide whether to allow the spending of the transaction 

- scripts have access to the entire transaction being validated
  - can see all inputs
  - can see all outputs

- a `datum` can be associated with a utxo

- `scriptContext` is given to a script which contains all the inputs and all the outputs of a given transaction

### Who has to produce the datum, redeemer, and validator(script that evaluates spending)?
- rule in Plutus is that the spending transactions must do that, where as the transaction owned by the script(refered to as the 'producing transaction') only has to attach the hashes
- when a transaction produces a utxo that is owned by a script address, the transaction must produce the hashes of the `datum`, and the hash of the `script`
    - optionally it can include both in full as well
- when a transaction wants to spend a utxo that is owned by a script, it must provide the `script`, `datum` and `redeemer` in full
- this means that in order to spend a given utxo, you need to know the datum, because only the hash is visible on the blockchain(from the producing transaction)
  - optionally this can be bypassed in the case the producing transaction includes the datum in full

side note:  
- sending a utxo to a script address without a datum makes it impossible to spent that utxo
   - use unit type if you don't need a datum


# Hashing and digital signatures

## hashing:

### Algorithms: 
SHA-256: 32bit length hashing

- output of hashes will always be the same length
- no way to predict what output a given input will have
- practically impossible to reverse engineer
- most blockchains rely heavily on the fact that it is impossible to find the input text for a given hash

## Digital signatures:

### Algorithms: 
RSA
ECDSA

- allows proof of digitally signing documents
- requires a public and private key pair
- public key can be derived from the private key, but not the other way around
- given the signature, public key, and payload, the receiver can verify that the payload was signed by the public key
- signature is tied to payload
- signature is tied to public key

