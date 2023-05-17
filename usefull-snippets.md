# Useful code snippets

### Int to builinByteString 
```hs

{-# INLINEABLE intToBuiltinByteString #-}
intToBuiltinByteString :: Integer -> BuiltinByteString
intToBuiltinByteString i = encodeUtf8 $ intToString i

{-# INLINEABLE intToString #-}
intToString :: Integer -> BuiltinString
intToString i = foldr appendString "" strings
  where
    ints = intToInts i
    strings = map intToChar ints

{-# INLINEABLE intToInts #-}
intToInts :: Integer -> [Integer]
intToInts i
  | equalsInteger a 0 = [b]
  | otherwise = intToInts a ++ [b]
  where
    (a, b) = divMod i 10

{-# INLINEABLE intToChar #-}
intToChar :: Integer -> BuiltinString
intToChar i
  | equalsInteger i 0 = "0"
  | equalsInteger i 1 = "1"
  | equalsInteger i 2 = "2"
  | equalsInteger i 3 = "3"
  | equalsInteger i 4 = "4"
  | equalsInteger i 5 = "5"
  | equalsInteger i 6 = "6"
  | equalsInteger i 7 = "7"
  | equalsInteger i 8 = "8"
  | equalsInteger i 9 = "9"
  | otherwise = "0" -- not possible
```

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
