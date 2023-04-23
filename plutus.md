# General plutus notes

### common imports 
```haskell
import           Plutus.V2.Ledger.Api      (
                                            BuiltinData, 
                                            POSIXTime, 
                                            PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, 
                                            from, 
                                            mkValidatorScript, 
                                            TxOut (txOutValue),
                                            TxOutRef, 
                                            Value,
                                            MintingPolicy,
                                            mkMintingPolicyScript
                                            )

import           PlutusTx                  (
                                            applyCode, 
                                            compile, 
                                            liftCode,
                                            makeLift, 
                                            BuiltinData, 
                                            compile, 
                                            unstableMakeIsData
                                            )

import           Plutus.V2.Ledger.Contexts (txSignedBy)

import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (.))

import           Prelude                   (IO)

-- time
import           Plutus.V1.Ledger.Interval (contains)

-- testing
import           Plutus.Model              (Ada (Lovelace), DatumMode (HashDatum),
                                           Run, Tx,
                                           TypedValidator (TypedValidator),
                                           UserSpend, ada, adaValue,
                                           defaultBabbage, initMock, mustFail,
                                           newUser, payToKey, payToScript,
                                           runMock, spend, spendScript, submitTx,
                                           toV2, userSpend, utxoAt, valueAt, waitUntil, currentTimeRad, validateIn)

-- util
import           Utilities                 (wrapValidator, writeValidatorToFile) -- local file
```

### common LANGUAGE options
```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NumericUnderscores    #-}
```

- `{-# LANGUAGE DataKinds             #-}` required for the `$$(compile [|| ... ||])` code 
- `{-# LANGUAGE ScopedTypeVariables   #-}` required to use `makeLift ''Params`
- `{-# LANGUAGE MultiParamTypeClasses #-}` required to use `makeLift ''Params`
- `{-# LANGUAGE NoImplicitPrelude     #-}` ?
- `{-# LANGUAGE OverloadedStrings     #-}` required to work with string literals
- `{-# LANGUAGE TemplateHaskell       #-}` required for `[|| ... ||]` code
- `{-# LANGUAGE NumericUnderscores #-}` allows using numbers like `10_000_000`

### common INLINABLE flags

```haskell
--make inlinable to use in mkWrappedValidator
--    vvvvv
{-# INLINABLE mkValidator #-}
mkValidator :: DatumSwap -> () -> ScriptContext -> Bool
mkValidator ds _ ctx = True

-- make inlinable to use in validator
--         vvvvvv
{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator


validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])
--                                              ^^^^^
--                                   inlinable mkWrappedValidator
```

#### helpful validator(global) functions


- Get the public key hash that locks the transaction output, if any.  
`pubKeyOutput :: TxOut -> Maybe PubKeyHash`

- Get the list of TxOut outputs of the pending transaction at a given script address.  
`scriptOutputsAt :: ValidatorHash -> TxInfo -> [(OutputDatum, Value)]`

- Get the values paid to a public key address by a pending transaction.  
`pubKeyOutputsAt :: PubKeyHash -> TxInfo -> [Value]`

- Get the total value locked by the given validator in this transaction.  
`valueLockedBy :: TxInfo -> ValidatorHash -> Value`

- Get the total value paid to a public key address by a pending transaction.  
`valuePaidTo :: TxInfo -> PubKeyHash -> Value`

- Check if the pending transaction spends a specific transaction output (identified by the hash of a transaction and an index into that transactions' outputs)  
`spendsOutput :: TxInfo -> TxId -> Integer -> Bool`

- Check if a transaction was signed by the given public key.  
`txSignedBy :: TxInfo -> PubKeyHash -> Bool`

- Get the total value of inputs spent by this transaction.  
`valueSpent :: TxInfo -> Value` 

- Get the total value of outputs produced by this transaction.  
`valueProduced :: TxInfo -> Value`

- The CurrencySymbol of the current validator script.  
`ownCurrencySymbol :: ScriptContext -> CurrencySymbol`

- Get the validator and datum hashes of the output that is curently being validated  
`ownHashes :: ScriptContext -> (ValidatorHash, OutputDatum)`

- Get the hash of the validator script that is currently being validated.  
`ownHash :: ScriptContext -> ValidatorHash`

- Convert a CurrencySymbol to a ValidatorHash  
`fromSymbol :: CurrencySymbol -> ValidatorHash`
