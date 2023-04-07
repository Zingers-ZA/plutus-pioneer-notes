https://www.youtube.com/watch?v=Rnyc5YXVXew&list=PLNEK_Ejlx3x2zXSjHRKLSc5Jn9vJFA3_O&index=7&ab_channel=IOGAcademy

- usually a transaction that is trying to spend a utxo that is owned by a script address, must provide the script (serialized to cbor)
- the node that then processes this transaction hashes the script provided and checks that the hash matches the hash attached to the utxo(that is to be spent)
- it then executes the script

### the Vasil hardfork introduced reference scripts

- this allows a spending transaction to specify a reference utxo that contains a script(serialized to cbor), for the evaluating node to look at
- this means that the utxo does not need to include the entire script, more so a pointer to an existing script

<b>so where do you store the utxo that contains the actual script?</b>
- possibly at a burn address, where it can never be spent, guaranteeing that it will exist forever

### deploying a script:
it becomes quite simple then 

1. serialize the script to cbor
2. attach the script to a utxo
3. send that utxo to a burn address
4. use that script as a script reference in transactions
