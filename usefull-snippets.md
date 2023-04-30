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
consumesInputWithToken :: Bool
consumesInputWithToken = assetClassValueOf (txOutValue someInput) (AssetClass(someCurrencySymbol, someTokenName)) == 1

mintsUtxoWithToken :: Bool
mintsUtxoWithToken = assetClassValueOf (txInfoMint txInfo) (AssetClass(ownCurrencySymbol ctx, myName)) == 1
```

### Check outputs that are paid to validator 
```haskell
ownOutputs :: [TxOut]
ownOutputs = getContinuingOutputs ctx

-- incase only expect one
ownOutput :: TxOut
ownOutput = case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected only one output at script"
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
On the frontend:
```ts
const OracleRedeemer = Data.Enum([
    Data.Literal("Update"),
    Data.Literal("Delete"),
])
type OracleRedeemer = Data.Static<typeof OracleRedeemer>;
```
