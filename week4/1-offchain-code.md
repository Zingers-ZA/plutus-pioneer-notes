# On chain vs Off chain Code

### on chain
- code that runs in the node, when validating a transaction
- much more important that off chain, governs money
- unsures integrity
- expensive
- responsible for:
    - validating a transaction is allowed to spend certain utxo's

### off chain
- code that runs in a user/servers device, to query the blockchain or create transactions, etc. 
- responsible for:
    - finding and choosing which utxos to spend
    - creating transaction
    - getting transactions signed 
    - submitting transaction


### On chain code compilation pipeline
`PlutusTx` -> `GHC Core` -> `PIR(plutus intermediate representation)` -> `PLC(Plutus core)` -> `Untyped Plutus core`
- the code that is executed by the nodes is Untyped Plutus Core
    - meaning you an theoretically compile any language to this language if you create a compiler for it. 

### high level steps to deploy scripts 

1. create `script address`
    - this is the address that the script utxo will sit at
    - assuming we have the serialized script, we can use `cardano-cli address build`
    - only have to do this once
2. build the `transaction` that deploys
    - indicate the inputs and outputs
        - inputs should include at least one utxo from the signer that can cover the fees
        - outputs should include at least one at the script address, and one at the signers address(for the change)
    - `cardano-cli transaction build/build-raw`
3. providing the `datum`
    - a datum must be provided for the utxo that sits at the script address
    - 3 options:
        1. add datums hash in utxo
        2. add datums hash in utxo + the entire datum in tx body
        3. include the entire datum in the utxo (inline datum)
    - there are tradeoffs
        - putting the actual datum in the utxo is more costly, but if you use this more than once it might be cheaper this way
4. providing the `script`
    - 2 options
        1. provide only script hash (the payment part of the script address)
            - this means that providing the actual script(validator code) is the job of the consuming transaction
            - the script that is provided by the consuming transaction will be hashed and checked against the hash provided in the producing transaction(this transaction)
        3. include the entire script
            - more expensive
            - if you use the script a lot it might make sense because otherwise every consuming transaction would need to provide it 
5. Coin selection, balancing, calculation of fees
    - this is usually done automatically by most tools
        - `cardano-cli transaction build`
        - `lucid`
        - etc.

### high level steps to use scripts (spending transaction)
1. indicate inputs and outputs(utxos)
    - option to use `reference utxos`
        > remember that the context provided to the script is only it's inputs and outputs.
        > Sometimes it might need references to utxo's that it wont spend just to see what's in them

2. Providing `datum`
    - there are 3 options depending on how the validator was deployed
        1. the utxo at the validators address has the `datum hash` only 
            - spending transaction will need to provide the datum
            - this requires the transaction to know the datum
        2. utxo has datum in `tx body`
            - need to retrieve it from the blockchain and provide it 
        3. utxo has the datum as an inline datum
            - easiest
            - spending transaction must just indicate that the script utxo has an inline datum 
3. Providing `script`
    - 2 options
        1. no utxo has the script attached
            - must have some way to know the script and provide it
        2. attached the script to a utxo 
            - this allows you to provide the script as a reference script
                - script must pass to allow the transaction to be valid
            - advantages
                - actual scripts don't need to be included in every spending tx 
                - cheaper spending transactions
                - easier to run more scripts without reaching max tx size
4. provide redeemer
    - spending transaction must provide the redeemer
5. provide collateral
    - just a utxo to pay for script execution incase of failure
    - collateral is only consumed(partially) if script fails

### Signing 
- `cardano-cli transaction sign`
- all transactions must be signed by at least one signer
    - this is because all transactions must have at least one utxo, which require the signature of the owner to spend
- theoretically this is the only step that is required to run on the users machine

### Submitting
- `cardano-cli transaction submit
- submitting a transaction must pass `2 validation phases` before being added to the chain
    - to minimize the resources the node needs to use and prevent unnecessary costs for the user
    1. check the tx was built correctly. 
        - inputs exist
        - pays correct fees
        - correct collateral
        - if this phase fails, the tx fails without costing fees or collateral
    2. runs all scripts included in the transaction
        - if the scripts validate the tx actions, the node will add the transaction to the ledge and collect fees
        - `all scripts must succeed`

### Determinism
- offchain code can fully evaluate whether a transaction will succeed before it ever reaches a node
- all details can be determined:
    - fees
    - failure/success
    - inputs/outputs

