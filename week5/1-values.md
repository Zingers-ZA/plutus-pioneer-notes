# Values in cardano

- utxo's store value, and up until now in the lectures value has only been in the context of ada(lovelace)
- if you want to create tokens other than lovelace, you must create them, or burn them 

### Value type:
```
Value 
    getValue :: Map CurrencySymbol (Map TokenName Integer)
                        ^^^              ^^^        ^^^
                'HOSKY' for eg.      HoskyToken    150
```
- token on cardano is defined by a `CurrencySymbol` and a `Name`
- CurrencySymbol and TokenName are both `BuiltInByteString`
- `AssetClass` is a type wrapper for a pair of CurrencySymbol and TokenName
    - Ada is an assetclass
    - all native tokens are also assetClasses
    - the value type above is isomorphic to an AssetClass because of the structure being roughly the same. ie. a map of CurrencySymbol + TokenName -> Amount
- Because ADA is the default value, the CurrencySymbol and Token Name are both '' for it

helper function called `assetClassValue`
```
assetClassValue :: AssetClass -> Integer -> Value
-- returns value containing an assetClass and amount of that assetClass
-- usage:

let ada = assetClass adaSymbol adaToken
assetClassValue ada 10000000

--> Value (Map [(, Map [("", 10000000)])])
```

helper function called `assetClassValueOf`
```
assetClassValueOf :: Value -> AssetClass -> Integer
-- returns amount of tokens of a given assetclass are within a 'Value'
-- usage:

let ada = assetClass adaSymbol adaToken
let v = assetClassToValue ada 10000000

assetClassValueOf v ada

--> 10000000
```

## Monoid:
- Value implements `monoid` 
- monoid is a class that exposes some functions for appending things, amongst other stuff
    - mappend - concat things, indicated by `<>` (bind)
    - mempty - the neutral element in a monoid
        - 'if you combine it with another element it doesn't change that element'
        - in strings : ""
        - in Sum: 0
        - in Product: 1 
        in List: []
    - mconcat 
the idea is that it works with multiple types, eg:
```
"Plutus" <> "Pioneer" :: String
        ^^^^
      mappend
```

## combining Values

- Since value implements `monoid`, there are mappend, mempty and mconcat operations present on it.
- means that if you want add 2 values, you can use `<>`
eg:
```
let myToken = assetClassValue (AssetClass "fd23eab2" "myToken" ) 100
let yourToken = assetClassValue (AssetClass "cfb2eab2" "yourToken" ) 200

let v = myToken <> yourToken
                ^^^
            adds here
-- > Value ( Map [ (fd23eab2, Map[("MyToken") ]), (cfb2eab2, Map[ ("yourToken", 200) ]) ])
```
- `PlutusTx` library has a monoid which has a `gsub` operation
    - allows subtracting an amount of an assetClass from a `Value`
    - same syntax as above, `gsub myValue $ assetClassValue ada 1000000`

## analyzing Values

- there are functions to compare values
- because values can be different assetClasses, these functions only compare like assetClasses. 
- `gt :: Value -> Value -> Bool` greater than. for every coin in the first value, the second value has more of that coin
- `geq :: Value -> Value -> Bool` greater or equal. for every coin in the first value, the second has more of equal 
- `lt`
- `leq`
- `isZero :: Value -> Bool` value is zero    

