## Example vesting contract: 
```
data VestingDatum = VestingDatum            <--- custom datum type
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    }

unstableMakeIsData ''VestingDatum           <-- needed to allow BuiltInData instance

{-# INLINABLE mkVestingValidator #-}
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                                traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx            < -- using the ScriptContext to get TxInfo

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info
--                                 ^^                     ^^^^^ 
--      checks validTime is within deadline-Infinity      getting the valid time range from txInfo

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
```

Notes:
- the above code checks that 
    1. the tx is `signedBy` the beneficiary, 
    2. the tx's `TxInfoValidRange` is entirely after the deadline
- since we are in the validator script, we know that we are within `TxInfoValidRange`
- must always make sure that the entire of the `TxInfoValidRange` is within an acceptable range
    - if you did not do this, some piece of the txInfoValidRange might fall outside of the acceptable deadline and could possibly allow the script to allow the spend
