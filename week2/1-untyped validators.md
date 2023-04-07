https://www.youtube.com/watch?v=3tcWCZV6L_w&list=PLNEK_Ejlx3x1-oF7NDy0MhXxG7k5O6ZOA&ab_channel=IOGAcademy

When a transaction is submitted to a node that tries to spend a utxo owned by a script, the node executes the `validator` script and uses the result of the script to determind whether the utxo can be spent

`datum`, `redeemer` at a low level are both of type `BuiltInData` in plutus

- the way to get the value of the buildinData type is normally to use `dataToBuiltInData` or `builtInDataToData` functions
- this produces a `data` type

## 'Data' type
- mimics the structure of json
- has constructors for Int, String, Map[(Data, (Data))], and List [Data]
- this allows you to replicate json structures in plutus 


## Simple validator script

```
--   Name            Datum         Redeemer     ScriptContext   Output Type
mkGiftValidator :: BuiltInData -> BuiltInData -> BuiltInData -> ()
mkGiftValidator _ _ _ = ()
```
notes:   
3 underscores me we don't care about the inputs   
returns () 

- the way validators work at a low level is that if there is no error then validation passes, otherwise fails
- `script addresses` are the hash of the `validator`
    - if you turn a plutus validator and compile it to plutus script, the has of that script is the script address

## Compiling to plutus core script

```
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])
```

- this converts a plutus validator to plutus core script
- the `[|| ... ||]` syntax takes the function you give it and effectively generates the source code of that function and inserts it in place
- `compile` takes that code and produces Plutus core `syntax` from it
- `$$` takes this syntax and splices it to compiled code
- `mkValidatorScript` takes that Plutus Core and produces a validator from it
- this script is expecting a function signature of `BuiltInData -> BuiltInData -> BuiltInData -> ()`

