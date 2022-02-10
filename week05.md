# Week05 - Minting Policy
Minting, burning, sending tokens

## 1. Values:

Value Type:
```
Value 
    getValue :: Map CurrencySymbol (Map TokenName Integer)
```
- Map from CurrencySymbol to a Map of TokenNames to Integers
- Each native token is identified by a CurrencySymbol and TokenName
- A value just says how many units of each AssetClass are contained in it
```
CurrencySymbol
    unCurrencySymbol :: BuiltinByteString
TokenName
    unTokenName :: BuiltinByteString
```
- CurrencySymbol is just a type wrapper around a bytestring
- TokenName is just a type wrapper around a bytestring

<b>AssetClass:</b>
```
AssetClass
    unAssetClass :: (CurrencySymbol, TokenName)
```
- wrapper around the pair 
- Ada will be one asset class, other native tokens will be another asset class
- `getValue :: Map CurrencySymbol (Map TokenName Integer)` is equivilent to a map between Integers and AssetClasses
    - which means that <b>a value just says how many units of each AssetClass are contained in it</b>

Ada Types:

`adaSymbol :: CurrencySymbol  - ""`  
`adaToken :: TokenName        - ""`

Function for Lovelace:  
`lovelaveValueOf 124` returns `Value (Map [(, Map[("", 124)])])`
- The above type is a Value. Notice the outer map contains the CurrencySymbol(which is blank), and the inner contains the TokenName(also blank) and amount

Adding loveLace:  
`lovelaceValueOf 123 <> lovelaceValueOf 10`
- <b>Value class is a monoid</b>, which means it implements mappend, which can be written as `<>`


#### Singleton:
Creating values containing native tokens:  
`singleton :: CurrencySymbol -> TokenName -> Integer -> Value`
- Contsruct a value, consisting of one `AssetClass` and the Int specifies the amount of that token  
- <b>NB</b>: The CurrencySymbol must be a string representing a hexadecimal value
    - The reason for this is because it is the hash of the minting policy
- Because singleton returns a Value, it also implements mappend and can be added with `<>`  
eg:   
`singleton "a8ff" "ABC" 7` = 
`Value (Map [(a8ff, Map[("ABC", 7)])])`  

#### Adding multiple different tokens:

`singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100` =   

`Value (Map [(,Map [("", 42)]), (a8ff, Map [("ABC", 7), ("XYZ", 100)])])`
- The outer map now contains 2 entries:
    - One for the lovelace, which is the map with blank names and a value of 42
    - One for the `a8ff` currency, with a map for each of the `TokenNames` and each of their values

#### ValueOf: Extract amount of tokens in value:
`valueOf :: Value -> CurrencySymbol -> TokenName -> Integer`
- Returns the amount of tokens inside a value that match the CurrencySymbol and TokenName requested
- Returns 0 if no matches
```
let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
valueOf v "a8ff" "XYZ"                   - 100
valueOf v "a8ff" "ABC"                   - 7
valueOf v "a8ff" "abc"                   - 0
``` 

#### FlattenValue:
`flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]`
- Flattens a map value into a list of triples
```
flattenValue v                           - [(a8ff, "ABC", 7), (a8ff, "XYZ", 100), (,"", 42)]
```

### Minting Policies:
The reason for the CurrencySymbol being a hex value, is because it is the hash of the minting policy for that symbol
- If a tx wants to mint or burn tokens, the minting policy is looked up for each token
    - The minting policy is the hash of a script
    - The script must also be included in the tx
    - That script is then executed along with the other validation scripts
    - The purpose of these validation scripts is to decide whether this tx can mint/burn these tokens

