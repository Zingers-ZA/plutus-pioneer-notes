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

import           PlutusTx                  (applyCode, compile, liftCode,
                                            makeLift)

import           Plutus.V2.Ledger.Contexts (txSignedBy)

import           PlutusTx.Prelude          (Bool, traceIfFalse, ($), (&&), (.))

import           Prelude                   (IO)

-- time
import           Plutus.V1.Ledger.Interval (contains)

-- testing
import           Plutus.Model            (Ada (Lovelace), DatumMode (HashDatum),
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
