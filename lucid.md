# Lucid

# General:

- lucid can be used with blockfrost, but also any provider that you can host yourself
- can get the docs from at https://deno.land/x/lucid@0.9.8/mod.ts
- importing
    - `import * as L from "https://deno.land/x/lucid@0.9.8/mod.ts";`
    - `import Lucid from "https://deno.land/x/lucid@0.9.8/mod.ts";`

# Connecting to a wallet

- selecting a wallet in lucid loads the wallet into the global `lucid.wallet` property 
    - `selectWallet` used mainly on the browser(finds `window.cardano.<walletname>.enable`)
    - `selectWalletFromSeed` gets a wallet from a string of words(seed phrase format)
    - `selectWalletFromPrivateKey` gets a wallet from a private key

# Addresses:

- `lucid.wallet.address()` returns string address of currently loaded wallet

# Utxos:

```
const vestingScript: SpendingValidator = {
    type: "PlutusV2",
    script: "..."
};

const addr = lucid.utils.validatorToAddress(vestingScript)

// or const addr = lucid.wallet.address()

await lucid.utxosAt(addr); // [UTxo]
```

# 'Data':

- `Lucid.Data` gives access to all serializable BuiltInData types
    - `Data.Bytes()`
    - `Data.Integer()` 
    - `Data.Map()`
    - `Data.void()` for haskell unit
    - etc.
- `Data` makes working with cbor much easier
    - `Data.to<type>(object, shape)` allows converting an object to cbor string, given it's shape and type
    - `Data.from<type>(string, shape)` allows converting an cbor string to an object, given it's shape and type

Must be used for datum/redeemer/etc.
```
const MyDatum = Data.Object({
  name: Data.Bytes(),
  age: Data.Integer(),
  colors: Data.Array(Data.Bytes()),
  description: Data.Nullable(Data.Bytes()),
});
type MyDatum = Data.Static<typeof MyDatum>;
```
- this is only illustrating the shape of the datum
- when you provide this in a transaction, you must convert strings to bytes:
```
  const datum: MyDatum = {
    name: fromText("Lucid"),
    age: 0n,
    colors: [fromText("Blue"), fromText("Purple")],
    description: null,
  };
```
submitting: 
```
const tx = await lucid                     type here     json      Data.Object
  .newTx()                                      vvvv     vvv       vvv
  .payToAddressWithData("addr_test...", Data.to<MyDatum>(datum, MyDatum), {
    lovelace: 10000000n,
  })
  .complete();
```
#### helpers
- 



# transactions

- transactions can be in 3 states
    1. under construction
        - `Tx` type
            - contains functions to build transactions 
                - `.addSignerKey` - flag required signature
                - `.collectFrom` - input utxos
                - use `.complete()` at the end to create a `Promise<TxComplete>` from a `Tx`
    2. constructed but not signed
        - `TxComplete`
            - contains functions to sign transactions
                - `.sign` - request signature
                - `.complete()` returns `Promise<TxSigned>`
    3. signed
        - `TxSigned`
            - contains functions to submit transactions
                - `.toString` - to hex encoded cbor 
                - `.submit()` - submit to blockchain, returns `Promise<TxHash>`

- example 1 (producing tx):
```
async function vestFunds(amount: bigint): Promise<TxHash> {
    const dtm: Datum = Data.to<VestingDatum>(datum, VestingDatum)
    const tx = await lucid
        .newTx()                                    <-- create tx
        .payToContract(vestingAddress, { inline: dtm }, { lovelace: amount })
        .complete()                                 <-- creates TxComplete
    const signedTx = await tx.sign().complete();    <-- creates signedTx
    const txHash = await signedTx.submit()          <-- creates txHash
    return txHash;
}
```
- example 2 (consuming tx):
```
async function claimVestedFunds(): Promise<TxHash> {
    const dtm: Datum = Data.to<VestingDatum>(datum,VestingDatum);
    const utxoAtScript: UTxO[] = await lucid.utxosAt(vestingAddress);
    const ourUTxO: UTxO[] = utxoAtScript.filter((utxo) => utxo.datum == dtm);
    
    if (ourUTxO && ourUTxO.length > 0) {
        const tx = await lucid
            .newTx()                                   <-- create tx
            .collectFrom(ourUTxO, Data.void())         <-- utxos to use
//                                ^^^^^^^^^
//                  redeemer for utxo(multiple if multiple utxos)
            .addSignerKey(beneficiaryPKH)              <-- specify a certain signature necessary    
            .attachSpendingValidator(vestingScript)    <-- attach script 
            .validFrom(Date.now()-100000)              <-- sets txInfo.txInfoValidRange
            .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        return txHash
    }
    else return "No UTxO's found that can be claimed"
}
```

#### transaction building

there are a few functions commonly used during tx building
- `.collectFrom` defines which utxos to consume
    - this adds inputs to `ScriptContext.txInfo.txInInfo`
- `.mintAssets` flags that the transaction will mint assets
    - this attaches info to `ScriptContext.txInfo.txInfoMint` in the background
- `.validFrom` sets the `ScriptContext.txInfo.txInfoValidRange` to start at a point and end at +infinity



# validators/scripts 

```
const vestingScript: SpendingValidator = {
    type: "PlutusV2",
    script: "..."
};
```
- the script '...' above would be the cbor hex of the actual script compiled to Untyped Plutus Core
- `lucid.utils.validatorToAddress(vestingScript)` here would give the address of the script

#### interacting with scripts

there are a couple functions useful for working with scripts:
- `attachSpendingValidator(vestingScript)`, this attaches a script to the transaction
- `attachMintingPolicy(policy)`, attaches minting policy script for purposed of minting
- `.addSignerKey(pkh)`, attaches signature of a signer to tx, this is required if Plutus scripts need to validate that a signer was present 
    - even if the signee of the entire tx is the required signee, plutus doesn't have context of that, so you must attach it again here

#### script parameters
quick recap:
- script params are compiled into the script before a transaction can produce an output that sits at that script address
- this means that if one of your params needs to come from the user, you need to compile the script on the fly with their param
- lucid does this with `applyParamsToScript`. Eg:
```
const addr: Address = await lucid.wallet.address();

const pkh: string = getAddressDetails(addr).paymentCredential?.hash || "";

const Params = Data.Tuple([Data.String]);
type Params = Data.Static<typeof Params>;

const signedPolicy: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript<Params>(
        "590..32fda",
        [pkh],
        Params)
};
```
- this applies a given set of params to a partially compiled script
    - partially compiled because this must be compiled without the params, so they can be applied later


# Minting

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









