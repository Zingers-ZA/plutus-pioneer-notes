# General

- follows the `eUTXO` model
    - `utxo` means that the state of the blockchains funds is tracked through unspent transaction outputs
    - utxos are owned by an address and they govern the assets that the address owns
    - transactions take utxos as input and create utxos as output
    - the `e` in eutxo refers to 'extended'
        - in normal block chains(like bitcoin), transactions that consume utxos are validated by checking that the 
          transaction is signed by all owners of the utxos being consumed
        - cardano allows for attaching arbitrary logic to this validation step

# scripts
- scripts are executed when a node validates a transaction, to confirm a transaction can spend a utxo
- data is passed in via a `datum`, `scriptContext`, and `redeemer`
    - `datum` provided by producing transaction
    - `scriptContext` is a combination of all inputs and all outputs of a transaction. Can include `reference inputs`
    - `redeemer` is provided by the spending transaction and is meant to provide data that unlocks the utxo

> `reference inputs` are inputs that provide context but will not be spent or altered

- the native language interpreted by cardano nodes(validators) is called `Untyped Plutus Core`
- any validator scripts must be complied to this language to be uploaded to the chain
    - this is what is done at the bottom of most haskell scripts in the plutus pioneers program
    - it can be done in other ways(eg. <b>aiken</b> can compile to this language as well)

## datums
- `datum` is a piece of arbitrary data that is attached to a utxo that will be produced and sit at a script address
- it is attached to a utxo
- it will be passed to script on validation

## script params
*Current understanding*
- script params are another way to provide data to a script
- unlike datum and redeemer, the values of script params are baked(hardcoded) into the script on compilation
- this means that the script hash will change, and therefore the script address
- this also means that the script params must be provided by the producing transaction, so that it will be able to provide the correct script address(by hashing the script including params)



# addresses



# blocks
