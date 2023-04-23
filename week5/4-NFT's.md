# NFT's
https://www.youtube.com/watch?v=9kW-z_RuwEY&list=PLNEK_Ejlx3x2T1lIR4XnDILKukj3rPapi&index=5&ab_channel=IOGAcademy

- how do you garauntee that a token can only be minted once? 
    - you only have context of the current transaction
- previously, in the Mary era, people used to use time locked policies
    - the minting script only allows minting before a certain time, afterwhich it 'locks'
- The way to do this is the make sure that the `minting script` can only `succeed once`.

#### But how? 
- We need something that is <b>unique</b> at the point of the mint
- The answer is `utxos`
    - <b>a utxo can only exist once, and is completely unique</b>
    - this also means that a given <b>transaction is also unique</b>
        - because it must contain at least one utxo to pay fees
- Therefore, if we can `bake a utxo into a script`, and then consume that utxo in the same transaction as the script, it will prevent the script from ever succeeding again since the utxo can never exist again.

```haskell
--         utxo as param    token name as param
--              vvvvvv       vvvv
mkNFTPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool                                                 --  make sure utxo is consumed by this tx
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == 1                  -- tokenName correct and amount = 1
        _                -> False
```
#### code required for compilation

```haskell
{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tid ix tn' = wrapPolicy $ mkNFTPolicy oref tn
  where
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ PlutusTx.unsafeFromBuiltinData tid)
        (PlutusTx.unsafeFromBuiltinData ix)

    tn :: TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

----------------------------------------------------
-- version to have params applied later
nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/nft.plutus" nftCode

---------------------------------------------------
-- fully compiled version including params
nftPolicy :: TxOutRef -> TokenName -> MintingPolicy
nftPolicy oref tn = mkMintingPolicyScript $
    nftCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)

saveNFTPolicy :: TxOutRef -> TokenName -> IO ()
saveNFTPolicy oref tn = writePolicyToFile
    (printf "assets/nft-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref) $
        tn') $
    nftPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs
```
#### lucid interaction with script
```typescript
const utxos = await lucid.utxosAt(addr);
const utxo = utxos[0];                        // find a utxo to use, can just be the first

const tn = fromText("PPP NFT");    // token name

const Params = Data.Tuple([Data.String, Data.BigInt, Data.String]);
type Params = Data.Static<typeof Params>;

const nftPolicy: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript<Params>(
        "5909...011",
        [utxo.txHash, BigInt(utxo.outputIndex), tn],  // params to be applied
        Params)
};

const policyId: PolicyId = lucid.utils.mintingPolicyToId(nftPolicy);    // gets policyId(minting policy script)

const unit: Unit = policyId + tn;

const tx = await lucid
    .newTx()
    .mintAssets({[unit]: 1n}, Data.void())                    // mint 1 asset of our AssetClass(tokenName+policyId)
    .attachMintingPolicy(nftPolicy)                           // attached minting policy
    .collectFrom([utxo])
    .complete();
```
