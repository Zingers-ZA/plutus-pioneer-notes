# Useful code snippets

### Get datum(inline or hash)
```haskell
parseDatum :: TxOut -> TxInfo -> Maybe Integer -- whatever type the datum returns 
parseDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> fromBuiltinData d
    OutputDatumHash h -> do
                        Datum d <- findDatum h info
                        fromBuiltinData d

```
- `findDatum` is a helper to find the datum by it's hash in the txInfo 

### Checking if utxo contains token/assetclass
```haskell

```

### Using redeemer as action
```haskell
data MyRedeemer = Update | Delete
    deriving Show
unstableMakeIsData ''OracleRedeemer

...

validator :: MyParams -> MyDatum -> MyRedeemer -> ScriptContext -> Bool
validator p d r ctx = case r of 
                Update -> ...
                Delete -> ...
...
```
