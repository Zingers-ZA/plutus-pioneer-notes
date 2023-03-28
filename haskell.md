# Haskell Notes:

Haskell unit type: `()` - no arguments, one value, unit
- similar to void type in other languages

- Setting a var in a do command `let pkh = mockWalletPaymentPubKeyHash $ knownWallet 2`. (no `in` required)


### Types:

- `newType` creates new type


### Data and accessors:
```
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
```
- beneficiary can be accessed via `beneficiary VestingDatum`


### functions: 
```
     name   arg1Type arg2Type outputType
    myFunc :: ...   -> ... -> ...
    myFunc     _      _   =   ()
               ^      ^        ^ 
          arg1Name arg2Name returnValue
```
calling a function:
```
myFunc "hello"
```

### Multiple constructors:
```
myFunc :: ... -> ... -> ...
myFunc Nothing  _ = Nothing
myFunc (Just x) f = f x
```

### Case:
```
foo :: .. -> .. -> .. -> ..
foo x y z = case <expression> of 
    <option1> -> <output1>
    <option2> -> <output2>
```

### Inline style:
- Any method calls can be written in inline style, eg:  
`elem 1 [1,2,3]` is the same as `1 'elem' [1,2,3]`

### Lambdas:
`(\x -> x + 1) 4` = `5 :: Integer`
- backslash means lambda

- Example of storing output in lambda "vars":
```
bar :: IO () 
bar = getLine >>= \s -> 
      getLine >>= \t ->
      putStrLn (s ++ t)
```

### Either:
`type Either :: * -> * -> *`  
`data Either a b = Left a | Right b`
- type to contain either one type or another type  
eg:   
`Left "Haskell" :: Either String Int` returns `Left "Haskell`  
`Right 7 :: Either String Int` returns `Right 7`  



