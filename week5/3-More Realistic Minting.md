# More realistic minting
https://www.youtube.com/watch?v=Faru8_Br2Xg&list=PLNEK_Ejlx3x2T1lIR4XnDILKukj3rPapi&index=4&ab_channel=IOGAcademy

- it's more realistic that only the creator of a token should be able to mint
- we'll create a minting script where the pubkeyhash of the person signing the tx is used as a param
    - this results in the script being different for every pubkeyhash, meaning each person gets their own policy
- we'll use lucid to apply these params to a script which doesn't have them baked in yet

#### Generating the script: 
>- this is a minting policy but the same applies to spend validators
 >   - note that minting policies only take in ScriptContext and Redeemer usually(no datum)
```
                   param
                    vvvv
mkSignedPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkSignedPolicy pkh () ctx = traceIfFalse "missing signature" $ txSignedBy (scriptContextTxInfo ctx) pkh
```

To generate a script that allows lucid to apply the params at a later stage, we will use the wrapper and validator create functions like so:  
```
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


# Parameterized scripts and Lucid

- Scripts can be compiled in lucid
    - allows 'applying' params to a script
- useful if the param is specific to the transaction
- This requires the <b>compiled script in a form that does not have the parameters embeded,</b> which we did above
    - effectively a compiled script to accept a parameter that compiles to BuiltInData

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


