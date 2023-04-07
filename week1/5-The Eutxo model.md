https://www.youtube.com/watch?v=ulYDNaEKf4g&list=PLNEK_Ejlx3x3xFHJJKdyfo9eB0Iw-OQDd&index=5&ab_channel=IOGAcademy

# UTXO Model:

- unspent transaction outputs
- an address can own a utxo
- addresses can tranfer funds by spending(consuming) utxos and generating new ones with new owners
- the signatures of owners are used to determine whether the utxo is allowed to be spent

# ETUXO model:

- a script can own a utxo
- the logic to decide whether a utxo can be spent is determined by the arbitrary logic within the script

- the `redeemer` acts as the justification to allow spending of the utxo

- scripts have access to the entire transaction being validated
  - can see all inputs
  - can see all outputs

- a `datum` can be associated with a utxo

- `scriptContext` is given to a script which contains all the inputs and all the outputs of a given transaction

### Who has to produce the datum, redeemer, and validator(script that evaluates spending)?
- rule in Plutus is that the spending transactions must do that(in general), where as the producing transaction only has to attach the hashes
- when a transaction produces a utxo that is owned by a script address, the transaction must produce the hashes of the `datum`, and the hash of the `script`
    - optionally it can include both in full as well
- when a transaction wants to spend a utxo that is owned by a script, it must provide the `script`, `datum` and `redeemer` in full, but it also possible that it might not need to do this depending on how the utxo was produced.
- this means that in order to spend a given utxo, you need to know the datum, because only the hash is visible on the blockchain(from the producing transaction)
  - optionally this can be bypassed in the case the producing transaction includes the datum in full

side note:  
- sending a utxo to a script address without a datum makes it impossible to spent that utxo  <b>[later me - (is this true? why?)]</b>
   - use unit type if you don't need a datum


#### datums (added from newer lecture for more context)
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