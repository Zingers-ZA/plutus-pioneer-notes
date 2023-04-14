# General plutus notes

### common imports 
```
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
```
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NumericUnderscores #-}
```

- `{-# LANGUAGE DataKinds             #-}` required for the `$$(compile [|| ... ||])` code 
- `{-# LANGUAGE MultiParamTypeClasses #-}` ?
- `{-# LANGUAGE NoImplicitPrelude     #-}` ?
- `{-# LANGUAGE OverloadedStrings     #-}` required to work with string literals
- `{-# LANGUAGE ScopedTypeVariables   #-}` ?
- `{-# LANGUAGE TemplateHaskell       #-}` required for `[|| ... ||]` code
- `{-# LANGUAGE NumericUnderscores #-}` allows using numbers like `10_000_000`

### common INLINABLE flags

```
make inlinable to use in mkWrappedValidator
      vvvvv
{-# INLINABLE mkValidator #-}
mkValidator :: DatumSwap -> () -> ScriptContext -> Bool
mkValidator ds _ ctx = True

make inlinable to use in validator
            vvvvvv
{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator


validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedValidator ||])
                                                ^^^^^
                                     inlinable mkWrappedValidator
```