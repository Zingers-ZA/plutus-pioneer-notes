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
- <b>NB</b>: The CurrencySymbol must be a string representing a hexadecimal value(because it is the hash of the minting policy for that symbol)
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

## 2. Simple minting policy:

```
ScriptContext:
    scriptContextTxInfo :: TxInfo
    scriptContextPurpose :: ScriptPurpose

ScriptPurpose:
    Minting (CurrencySymbol)
    Spending (TxOutRef)
    Rewarding (StakingCredential)
    Certifying (DCert)
```

- In all the previous examples we have always used the 'Spending' ScriptPurpose 
- <b>A script becomes a minting script when the value of `txInfoMint` is not zero</b>

#### TxInfo:
```
TxInfo
    txInfoInputs :: [TxInInfo]
    txInfoOutputs :: [TxOut]
    txInfoFee :: Value
    txInfoMint :: Value
    txInfoDCert :: [DCert]
    ...
```

- `txInfoMint` can contain multiple currency symbols
    - each currency symbol is the hash of a script
    - <b>for each currency symbol that is going to be minted by this tx, the corresponding minting policy is looked up and executed</b>
    - all policies must pass, if one fails they will all fail
    - these minting policy scripts only have 2 inputs and the context(no datum)

### Minting Policy Validators:
```
mkPolicy :: () -> SciptContext -> Bool
mkPolicy () _ = True 

policy :: Scripts.MintingPolicy
policy :: mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])    - converts to plutus core

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
```
Notes:
- The above code allows all minting and burning of the CurrencySymbol associated with this policy
- There is no datum for minting policy
- Similar to a script validator you must compile minting policies to plutus core
- Everything inside the oxford brackets(`[||]`) must be pre compilable, use `{-# INLINABLE mkPolicy #-}`
- Similarly to how you can have parameterise script validators you can also have parameterized minitng policies

3. More Realistic Minting Policy:






