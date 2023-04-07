# Terminology
1. `producing transaction(producer)` the transaction which creates a utxo that will sit at a script address  
2. `spending/consuming transaction(consumer)` the transaction that consumes the utxo owned by a script  
3. `datum` arbitrary data defined at the point of the producing transaction
4. `redeemer` arbitrary data defined at the point of the spending transaction, 'unlocks output'
5. `script` validation logic that governs spending of utxos it owns
6. `script parameter` arbitrary data baked into the script at compilation to allow multiple uses for the same script code


# General

- follows the `eUTXO` model
    - `utxo` means that the state of the blockchains funds is tracked through unspent transaction outputs
    - utxos are owned by an address and they govern the assets that the address owns
    - transactions take utxos as input and create utxos as output
    - the `e` in eutxo refers to 'extended'
        - in normal block chains(like bitcoin), transactions that consume utxos are validated by checking that the 
          transaction is signed by all owners of the utxos being consumed
        - cardano allows for attaching arbitrary logic to this validation step

# addresses

All have `2 parts`
```
 ---------------------------------------------------------------------
|          <payment part>         |          <staking part>            |
|   pubkeyhash   or  scripthash   |    pubkeyhash   or   scripthash    |  
 ----------------------------------------------------------------------
```
1. `payment` part
    - governs who has the right to spend an owned utxo
    - can be either:
    1. `public key hash`
        - owned utxos can only be spent by transaction that contain a signature from the corresponding private key
    2. `script hash`
        - corresponding script is must return `True` during validation

2. `staking/delegation` part (optional)
    - governs who is entitled to the staking rewards
    1. `public key hash`
        - owner of private key is entitled to rewards
    2. `script hash`
        - corresponding script is executed for transactions involving staking

# scripts
- scripts are executed when a node validates a transaction, to confirm a transaction can spend a utxo
- when a transaction wants to produce a utxo locked by a script, it will create the utxo at an address of which the payment part is the hash of the script
- when validating a transaction that spends utxo's at a script address, the validator node will only have the script hash(read 'addresses' above), so the actual script must be provided by the consuming transaction  
    - the validator can hash this script to verify it matches the script part of the address
    - optionally, consuming transaction can reference a script from a transaction already on the blockchain
        > reference scripts are scripts already on the blockchain, which can be supplied instead of the actual script, to reduce space used. They are purely there to provide the actual script, the script hash in the address of the utxo is still what governs what the validator will use to validate the transaction
- data is passed in via a `datum`, `scriptContext`, and `redeemer`
    - `datum` defined at time of producing transaction (read below 'datum' notes)
    - `scriptContext` is a combination of all inputs and all outputs of a transaction. Can include `reference inputs`
        > reference inputs are inputs that provide context but will not be spent or altered
    - `redeemer` is provided by the spending transaction and is meant to provide data that unlocks the utxo
- the native language interpreted by cardano nodes(validators) is called `Untyped Plutus Core`
- any validator scripts must be complied to this language to be uploaded to the chain
    - this is what is done at the bottom of most haskell scripts in the plutus pioneers program
    - it can be done in other ways(eg. <b>aiken</b> can compile to this language as well)

#### script params (parameterized scripts)

- script params are another way to provide data to a script
- unlike datum and redeemer, the values of script params are baked(hardcoded) into the script on compilation
- this means that the script hash will change, and therefore the script address
- for this reason it's a good idea to imagine a `family of scripts`, where you can reuse the same script logic, but have different addresses for changes 
    in params that will alter the logic paths slightly. 
- this also means that the script params must be provided at some point before a transaction can make use of the script
    - either just-in-time - ie. compile the script on the client side just before the transaction that produces a utxo at the script address
    - or if you know all the possible values for the params, you can compile the script ahead of time

#### deploying scripts

This can be thought of in 2 ways
1. 'Deploying a utxo that is gate-kept by a script' 
    - the steps for this would be 
        1. hash script
        2. generate script address
        3. create utxo, including datum at script address
    - this script is now gate keeping that utxo
    - <b>NB: as always, the consuming transaction needs to provide the actual script, which will be used by the node, but also hashed and validated against the script hash of the utxo's address to prevent tampering</b>
2. Generating a utxo at a burn address(so it exists forever), which has the script attached, to use as a reference script
    - this prevents the consumer having to include the actual script in every it's consuming tx
    - utxo's still sit at the script address, with their datum
    - the consuming transaction provides the reference to a utxo that contains the script, which the validator fetches, after which it can do it's hash match check against the script address and execute the script


> Collateral - a utxo that the node can consume to cover fees for processing a script that failed. This should never need to be consumed

## datums
- `datum` is a piece of arbitrary data that is 'attached'(not necessarily but in most cases) to a utxo
- it will be passed to script on validation
#### 3 options for defining the datum
1. producer `attaches datum hash to utxo` -> consuming transaction contains `actual datum in tx body`
    - script can verify the datum provided matches the datum hash
    - requires the consumer to know the datum through some other means
    - cheapest for producer, hash is quite small, more expensive for consumers
2. producer `attaches datum hash to utxo`, and provides `actual datum in tx body` -> consuming transaction contains `actual datum in tx body`
    - similar to previous, except now datum is recorded on blockchain by producer
    - possible to find datum using chain indexer
    - more expensive
3. producer attaches a `full inline datum to the utxo itself` -> consumer provides `nothing`, only indicates it expects an inline datum
    - consumer doesn't need to provide datum
    - cheaper for consumer
    - most common

# Fees

- fees depened on 
1. the size of the transaction in bytes
2. the scripts that have to be run to validate the transaction
    - scripts use more fees if they 
    1. use more memory
    2. have more steps to execute

# blocks


