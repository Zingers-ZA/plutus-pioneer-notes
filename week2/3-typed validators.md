## High level, Typed validators
https://www.youtube.com/watch?v=GT8OjOzsOb4&list=PLNEK_Ejlx3x1-oF7NDy0MhXxG7k5O6ZOA&index=3&ab_channel=IOGAcademy

- using `BuiltInData` is not optimal for a typed language because we are just lumping every arg into a format similar to json, meaning you lose context of the type

below script checks that the redeemer == 42
```
mk42Validator :: () -> Integer -> -> PlutusV2.ScriptContext -> Bool
mk42Validator _ r _ = r == 42
```
- changed BuiltInData to actual types
- returning bool provides same logic and error or no error for validation
- typed validators have a significant performance impact

### new compliation required
The above code causes the usual validator compiler to break
```
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkGiftValidator ||])
                                                                ^^^^^^^^^^^^^^
```
- this was expecting a type signature of `builtInData -> builtInData -> builtInData -> ()`, but you need to tell it that it needs to expect the new types
- this is done with `wrap`
```
validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrap mkGiftValidator ||])
                                                                ^^^
```
- `wrap` takes the types you defined and wraps them as `builtInData -> builtInData -> builtInData -> ()`
- `wrap` uses a function called `unsafeFromBuiltInData` under the hood
- the above can also be written like this:

```
mkValidator :: () -> Integer -> Integer -> PlutusV2.ScriptContext -> Bool
mkValidator _ r _ = r == 42

wrappedVal :: BuiltInData -> BuiltInData -> BuiltInData -> ()
wrappedVal = wrap mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])
```

### Custom types
- Can use custom types for `datum` and `redeemer`
- Would need to convert that custom type to a `Data` type
  - would be manual
- Alternative is to use template haskell to do it:
```
newtype MySillyRedeemer = MkMySillyRedeemer Integer

PlutusTx.unstableMakeIsData ``MySillyRedeemer
                            ^^
                            Syntax for name of type
```
- The above creates an instance against the MySillyRedeemer Type
  - this is not the same as a normal 'instance' in other languages
  - creating an instance in haskell means that the type can now be used as the instance type

### using custom types in cardano-cli
- can write custom data types to a file using a provided function called `writeDataToFile`
- `writeDataToFile` takes an instance of toData and converts it to it's equivilent json structure

```
import PlutusTx.Builtins
import Utilities
writeDataToFile "silly.json" $ MySillyRedeemer 37
```
- the output file can then be 