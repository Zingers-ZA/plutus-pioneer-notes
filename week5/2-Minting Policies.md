# Minting Policies
https://www.youtube.com/watch?v=g_VoKPK-tk0&list=PLNEK_Ejlx3x2T1lIR4XnDILKukj3rPapi&index=4&ab_channel=IOGAcademy

- inside the `Value` type, the `CurrencySymbol` is a hex string, because it is the script hash of the `minting policy script`
- this `minting policy script` is what governs whether a transaction can mint or burn tokens
- this script is executed by nodes when validating transactions, and therefore the script must be attached to transactions that mint/burn
    - same process as normal script, validator hashes script to check it matches the CurrencySymbol(mint policy script hash) and then executes the script
    - reference scripts can be used as well, will need to decide if it makes sense since minting policies aren't used that often
> ada fits into this scheme as well, because the CurrencySymbol is a empty string, which corresponds to an empty script, 
> since no ada can ever be created or destroyed
- there is `no producing transaction` during minting
- for minting scripts, there is `no datum`, only script context and redeemer
    - a datum is defined at the time of the producing transaction, but in this case there is no producing transaction, only consuming

## script context for minting
- recall the structure of ScriptContext
```
ScriptContext 
    scriptContextTxInfo :: TxInfo
    scriptContextPurpose :: ScriptPurpose
```

#### scriptPurpose`   
- `spending TxOutRef`
- `minting CurrencySymbol`
- `Rewarding StakingCredential`
- `Certifying DCert`  

Previous examples have mainly used a script purpose of 'spending', but minting scripts will be used to mint/burn tokens

#### TxInfo

```
txInfo
  txInfoInputs :: [TxInInfo]
  txInfoOutputs :: [TxOut]
  ...
  txInfoMint :: Value         <--  read about Value in '1-values.md'
  ^^^^^^^^^^^^^^^
 info about minting

```

- `txInfo` contains all context about the transaction that is being validated
    - all inputs, all outputs
    - other stuff
- minting policies are triggered if the `txInfoMint` property of the `txInfo` is a non-null value
    - for each AssetClass inside the Value of txInfoMint, the corresponding minting policy script is run
- minting 

# example minting policy
```
                only 2 params
               vvv    vvvvvvv
mkFreePolicy :: () -> ScriptContext -> Bool
mkFreePolicy () _ = True
-- always mints


-- wrapping and compiling
{-# INLINABLE mkWrappedFreePolicy #-}
mkWrappedFreePolicy :: BuiltinData -> BuiltinData -> ()
mkWrappedFreePolicy = wrapPolicy mkFreePolicy

freePolicy :: MintingPolicy
freePolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkWrappedFreePolicy ||])
```

# Minting with lucid

- effectively, need to create a script and include into a transaction that mints a Value with a CurrencySymbol that is the same as the script hash
- Lucid uses a type called `Unit` to represent `AssetClass`
- `fromText` is usefull to convert ascii(human) text to hex

```
const freePolicy: MintingPolicy = {
    type: "PlutusV2",
    script: "5830582e010000323222320053333573466e1cd55ce9baa0024800080148c98c8014cd5ce249035054310000500349848005"
};

const policyId: PolicyId = lucid.utils.mintingPolicyToId(freePolicy);
                                        ^^^^^
                       gets policyId(script hash) from minting policy

const unit: Unit = policyId + fromText("PPP Free");
           ^^^             ^^^^^^
  create 'assetclass'    CurrencySymbol + tokenName

const tx = await lucid
    .newTx()
    .mintAssets({[unit]: 10000}, Data.void())         <-- minting assets: contains Value, and redeemer
                  ^^      ^^         ^^
            assetClass   amount      redeemer
    .attachMintingPolicy(freePolicy)                  <-- attach actual script to tx
    .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();

```

