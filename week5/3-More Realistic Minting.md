# Parameterized scripts and Lucid

- Scripts can be compiled in lucid
    - allows 'applying' params to a script
- useful if the param is specific to the transaction
- This requires the <b>compiled script in a form that does not have the parameters embeded</b>
    - effectively a compiled script to accept a parameter that compiles to BuiltInData

#### Generating the script: 
>- this is a minting policy but the same applies to spend validators
 >   - note that minting policies only have 2 normal params(ScriptContext and Redeemer)
```
                   param
                    vvvv
mkSignedPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkSignedPolicy pkh () ctx = traceIfFalse "missing signature" $ txSignedBy (scriptContextTxInfo ctx) pkh

mkWrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy pkh = wrapPolicy (mkSignedPolicy $ PlutusTx.unsafeFromBuiltinData pkh)

signedCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
signedCode = $$(PlutusTx.compile [|| mkWrappedPolicy ||])

------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveSignedCode :: IO ()
saveSignedCode = writeCodeToFile "assets/signed.plutus" signedCode

-- executing the below code writes this script(ready to have params applied) to a file
-- saveSignedCode
```

- The above is compiling a script which accepts a parameter that can compile to the BuiltInData type
    - note that it is not a valid validator until the parameter is applied

#### Applying the params in lucid
```
const pkh: string = getAddressDetails(addr).paymentCredential?.hash || "";

const Params = Data.Tuple([Data.String]);
type Params = Data.Static<typeof Params>;

const signedPolicy: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript<Params>(
        "590...011",
        [pkh],
        Params)
};

const tx = await lucid
    .newTx()
    ...
    .attachMintingPolicy(signedPolicy)
    .addSignerKey(pkh)
    .complete();
```


