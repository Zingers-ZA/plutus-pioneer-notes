## Paramaterized Contracts
https://www.youtube.com/watch?v=ZSKVu32c5eA&list=PLNEK_Ejlx3x2zXSjHRKLSc5Jn9vJFA3_O&index=4&ab_channel=IOGAcademy

In the previous step (3-a vesting example) we used the datum to pass the beneficiary and deadline to the script
This is not ideal since we know the rule is that any transaction that wants to spend the output at that script address would need to provide this datum as well
- it would be more optimal to pass these as script params  
This is done like so:
```
data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }
```

script then changes to: 
```
{-# INLINABLE mkParameterizedVestingValidator #-} -- v datum becomes Unit
mkParameterizedVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator params () () ctx =
--                               ^^    ^^
--                     pull in params   datum becomes unit

    traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary params         <-- params now used

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline params) $ txInfoValidRange info <-- params now used
```
## This changes the function signature

meaning that the validator compiler will need to be adapted to accept the param
- (because it expects `BuiltInData -> BuiltInData -> BuiltInData -> ()` signature)

### Step 1 - update the wrapper:
```
{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
--                                        note the new param
--                                         vvv
mkWrappedParameterizedVestingValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator
--                                                     ^^
--                                           dot syntax required here
```

### Step 2 - update the validator 
```
validator :: VestingParams -> Validator
validator p = mkValidatorScript $$(compile [|| mkWrappedParameterizedVestingValidator p ||])
--       ^^                                                                           ^^
      add p so types are correct                                                add p so types are correct
```

# problem:
there is a problem with the above code, even though the haskell compiler won't thing there is. 
- the `[|| .. ||]` syntax is a template haskell function that inserts compiled code as if it was written in place, called `splicing`
- when template haskell tries to `splice` a piece of code, it must know what the code is at compile time(otherwise it won't know what to generate)
- the code above won't know what `p` is at compile time because it is a param that is only know when it is passed to the script at runtime

# solution:
- since we are able to compile the validator script, if we could only compile `p` then we could apply the validator script to `p`
- because `p` will always be only data(no functions), it can be compiled at runtime using a type called `liftCode`
- once we have a `liftCode` of `p` at runtime, we can use a function called `'applyCode'` to apply the validator script to the `p` `liftCode`  

note: `Lift` can only be applied to data types, not function type   

`LiftCode` signature: `LiftCode :: a -> CompiledCodeIn`

## How to fix:
### step 1
```
validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode params)
--                                                                                                  ^^^^^^^^^^^^^^^^^^^^^^
--                                                            applying the compiled script to the params liftcode that is compiled at runtime
```

### step 2
We must make a Lift out of the VestingParams object
```
data VestingParams = VestingParams
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }

makeLift ''VestingParams
-- ^^^^^^^^^^^^^^^^
```

## Notes: 
- can have multiple params, in which case you need to chain several `applyCodes` to apply the params one after the other, and would need to lift those params as well
- deciding when to use params and when to use datum is 'abit of an art' according to Lars
    - <B>NB: since params are compiled with the script at runtime, the scriptHash will change with different params passed </b>
        - this means that for every param, you get a new script address since the script address is the script hash


