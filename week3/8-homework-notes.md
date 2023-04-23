## Homework Notes:
- PaymentPubKeyHash is an instance of lift, therefore no `PlutuxTx.makeLift ''Xparam` is required at top of file
- Still need to create Lift in typedValidator
- Eg. of typedValidator with PaymentPubKeyHash:
```haskell
typedValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Vesting
typedValidator ppkh = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ppkh)
    $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.wrapValidator @POSIXTime @()
```
- Elegant check of signatories:
```haskell
checkSig :: Bool
checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info
```



