# Lucid

## General:

- lucid can be used with blockfrost, but also any provider that you can host yourself
- can get the docs from 

## connecting to a wallet

- selecting a wallet in lucid loads the wallet into the global `lucid.wallet` property 
    - `selectWallet` used mainly on the browser(finds `window.cardano.<walletname>.enable`)
    - `selectWalletFromSeed` gets a wallet from a string of words(seed phrase format)

## addresses:

- `lucid.wallet.address()` returns string address

## Data:

- `Lucid.Data` gives access to all serializable BuiltInData types
    - `Data.String`
    - `Data.BigInt` 
    - etc.

## validators

```
const vestingScript: SpendingValidator = {
    type: "PlutusV2",
    script: "..."
};
```

`lucid.utils.validatorToAddress(vestingScript)` here would give the address of the script

## transactions

- transactions can be in 3 states
    1. under construction
        - `Tx` type
            - contains functions to build transactions 
                - `.addSigner` - flag required signature
                - `.collectFrom` - input utxos
                - use `.complete()` at the end to create a `TxComplete` from a `Tx`
    2. constructed but not signed
        - `TxComplete`
            - contains functions to sign transactions
                - `.sign` - request signature
                - `.complete()` returns `TxSigned`
    3. signed
        - `TxSigned`
            - contains functions to submit transactions
                - `.submit` - submit to blockchain
                - `.toString` - to hex encoded cbor