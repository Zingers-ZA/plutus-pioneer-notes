https://www.youtube.com/watch?v=dcoYrIyEI4o&list=PLNEK_Ejlx3x2zXSjHRKLSc5Jn9vJFA3_O&ab_channel=IOGAcademy

- When a script executes it must be given context to be able to validate that a transaction can spend a utxo
- the context in this case must be all of the input utxos and all of the output utxos

`ScriptContext` is a type with 2 fields:
1. `ScriptPurpose` - Minting | Spending | Rewarding | Certifying
  - Spending is what will be the case when a script executes
  - Spending is populated with a TxOutRef
2. `TxInfo`
  - Contains all the information about the transaction
    - `[TxInInfo]` - a list of all the inputs of the transaction
      - <b>TxInInfo</b> has data fields 
        1. `TxOutRef` - a pointer to the utxo that this tx wants to consume
        2. `TxOut`, the utxo being created
          - <b>TxOut</b> is made up of an Address, Value, Datum, and Script
          - the above Datum and Script can be hashes
  - <b>TxInfo</b> can also reference Inputs that won't be consumed by containing Inputs inside a field called `txInfoReferenceInputs`
    - this allows multiple tx's in the same block to view a utxo, as opposed to having to consume it each time, in which case each transaction could use the utxo once per block

- `[TxOut]` a list of the transaction outputs 
- `TxInfoRedeemers` 
- `TxInfoData` -  this is a map from DatumHash to Datum, and is where datums need to be included for spending transactions
- `TxInfoId` - the id of the transaction
- `txInfoMint` - Info about tokens being minted, if non null triggers minting validation