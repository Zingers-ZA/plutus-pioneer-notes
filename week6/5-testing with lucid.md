# Unit testing with Lucid(and quickcheck)

## usefull snippets:
```typescript
const emulator = new L.Emulator([{ address: address1, assets: { lovelace: 10000000000n } }, { address: address2, assets: { lovelace: 10000000000n}}]);
const lucid = await L.Lucid.new(emulator);
```
- `emulator.now()` get current POSIXTime in emulator
- `emulator.awaitSlot(10)` waits for slot
- `emulator.awaitBlock(10)` waits for block 
- `emulator.getUtxos(address1)` gets utxos at address
- ``
## Lecture

- lucid has an `Emulator` type that returns a `Provider`
    - `provider` is the same type that you initialise Lucid with(like using Blockfrost for example)
    - means we can create a mock version of the blockchain with Lucid
    - `const lucid = await L.Lucid.new(emulator);`

#### validator that will be tested:
- `datum` contains deadline which must pass before utxo can be consumed
- `redeemer` must be >= 0
```haskell
newtype CustomDatum = MkCustomDatum { deadline :: POSIXTime }
unstableMakeIsData ''CustomDatum

mkValidator :: CustomDatum -> Integer -> ScriptContext -> Bool
mkValidator (MkCustomDatum d) r ctx = traceIfFalse "expected a negative redeemer" $ r <= 0 &&
                                      traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        deadlineReached :: Bool
        deadlineReached = contains (from d) $ txInfoValidRange info
```

#### some boilerplate:
- sets up script, script address, datum and redeemer types
```typescript
const negativeRTimedValidator: L.SpendingValidator = {
  type: "PlutusV2",
  script: "590A...456BF1",
};

const negativeRTimedAddr: L.Address = await (await L.Lucid.new(undefined, "Custom")).utils.validatorToAddress(negativeRTimedValidator);

const NegativeRTimedDatum = L.Data.Object({ deadline: L.Data.Integer() });
type NegativeRTimedDatum = L.Data.Static<typeof NegativeRTimedDatum>;

const NegativeRTimedRedeemer = L.Data.Integer();
type NegativeRTimedRedeemer = L.Data.Static<typeof NegativeRTimedRedeemer>;
```

#### test helper functions: 

```typescript
// the function that given the context of lucid, the wallet,the datum and sends 10 ada to the script address.
async function sendToScript(
    lucid: L.Lucid,
    userPrivKey: L.PrivateKey,                 // using private key because only emulating
    dtm: NegativeRTimedDatum
  ): Promise<L.TxHash> {
  lucid.selectWalletFromPrivateKey(userPrivKey);                // select wallet
  const tx = await lucid                             inline datum           
    .newTx()                                        vvvvvvvvvvvvvvv
    .payToContract(negativeRTimedAddr, { inline: L.Data.to<NegativeRTimedDatum>(dtm,NegativeRTimedDatum) }, { lovelace: 10000000n })
    .complete();
  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();
  return txHash
}
```


```typescript
// the function that given the context of lucid and a negative redeemer, grabs funds from the script address.
async function grabFunds(
    lucid: L.Lucid,
    emulator: L.Emulator,
    userPrivKey: L.PrivateKey,
    dtm: NegativeRTimedDatum,
    r: NegativeRTimedRedeemer
  ): Promise<L.TxHash> {                               // create redeemer
  lucid.selectWalletFromPrivateKey(userPrivKey);        // vvvv
  const rdm: L.Redeemer = L.Data.to<NegativeRTimedRedeemer>(r,NegativeRTimedRedeemer);
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(negativeRTimedAddr);                   // get utxos at the script address
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.datum == L.Data.to<NegativeRTimedDatum>(dtm,NegativeRTimedDatum));
//                                  ^^^^^^^
//                              filter for utxos that use our datum
  if (ourUTxO && ourUTxO.length > 0) {
      const tx = await lucid
          .newTx()
          .collectFrom(ourUTxO, rdm)                 // collect from all found utxos with our datum
          .attachSpendingValidator(negativeRTimedValidator)
          .validFrom(emulator.now())                 // use emulator.now() instead of date.now()
          .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
      return txHash
  }
  else throw new Error("UTxO's Expected!")
}
```
- `Data.to<x>(value, type)` converts a javascript object created with lucid `Data` properties to a cbor encoded string
- `emulator.now()` retrieves the current POSIX time in the emulator

#### testing function
```typescript
async function runTest(dtm: NegativeRTimedDatum, r: NegativeRTimedRedeemer, n: number) {
  const user1: L.PrivateKey = L.generatePrivateKey();
  const address1: string = await (await L.Lucid.new(undefined, "Custom")).selectWalletFromPrivateKey(user1).wallet.address();
//                                          ^^^^^^^^^
//                                      lucid not intiallized fully because we only need it to get a wallet address

  const user2: L.PrivateKey = L.generatePrivateKey();
  const address2: string = await (await L.Lucid.new(undefined, "Custom")).selectWalletFromPrivateKey(user2).wallet.address();

  // Setup the emulator and give our testing wallet 10000 ada. These funds get added to the genesis block.
  const emulator = new L.Emulator([{ address: address1, assets: { lovelace: 10000000000n } }, { address: address2, assets: { lovelace: 10000000000n}}]);
  const lucid = await L.Lucid.new(emulator);
//                       ^^^^^^
//                  lucid initalized here
  await sendToScript(lucid,user1,dtm);

  emulator.awaitSlot(n);

  await grabFunds(lucid,emulator,user2,dtm,r);

  emulator.awaitBlock(10);

  console.log(await emulator.getUtxos(address2));
}
await runTest({deadline:BigInt(Date.now()+20000*5+1000)},-42n,5*20);
```
- Lucid is initialized with `const lucid = await L.Lucid.new(emulator)`, which means that the Provider is the emulator, so we are using a simulated blockchain
- test takes in a datum and redeemer, sets up 2 users, then
    1. Submits a producing transaction, that creates a utxo with a deadline from `dtm` given in the params
    2. waits a period of time `n` given in the params
    3. submits a consuming transaction with redeemer `r` given in the params

#### Units tests
```typescript
function testSucceed(
  str: string, // the string to display of the test
  r: bigint,   // the redeemer number
  d: bigint,   // the deadline in seconds from now
  n:number     // the number of slots user 2 waits
) {
  Deno.test(str, async () => {await runTest({deadline:BigInt(Date.now())+d},r,n)})
}

async function testFails(
  str: string, // the string to display of the test
  r: bigint,   // the redeemer number
  d: bigint,   // the deadline in seconds from now
  n:number     // the number of slots user 2 waits
) {
  Deno.test(str,async () => {
    let errorThrown = false;
    try {
      await runTest({deadline:BigInt(Date.now())+d},r,n);
    } catch (error) {
      errorThrown = true;
    }
    assert(
      errorThrown,
      "Expected to throw an error, but it completed successfully"
    );
  });
};

// deadline is slot 100 and user 2 claims at slot 120
testSucceed("UT: User 1 locks and user 2 takes with R = -42 after dealine; succeeds",-42n,BigInt(1000*100),120);
// deadline is slot 100 and user 2 claims at slot 120
testSucceed("UT: User 1 locks and user 2 takes with R = 0 after dealine; succeeds",0n,BigInt(1000*100),120);
// deadline is slot 100 and user 2 claims at slot 120
testFails("UT: User 1 locks and user 2 takes with R = 42 after dealine; fails",42n,BigInt(1000*100),120);
// deadline is slot 100 and user 2 claims at slot 80
testFails("UT: User 1 locks and user 2 takes with R = -42 before dealine; fails",-42n,BigInt(1000*100),80);
// deadline is slot 100 and user 2 claims at slot 80
testFails("UT: User 1 locks and user 2 takes with R = 0 before dealine; fails",-0n,BigInt(1000*100),80);
// deadline is slot 100 and user 2 claims at slot 80
testFails("UT: User 1 locks and user 2 takes with R = 42 before dealine; fails",42n,BigInt(1000*100),80);
```

### property testing with fast check
- fastCheck is a javascript library, nothing to do with plutus/lucid
- it has helper functions to generate a lot of arbitrary data within given types, to be able to test many variations input to a function

#### describing how fast check should generate test values

```typescript
// set up a fixed deadline at slot 100
const dl: number = 100*1000;
// create only random 256 bit negative big integers for r.
const negativeBigIntArbitrary = fc.bigIntN(256).filter((n:bigint) => n <= 0n);
// ...
const positiveBigIntArbitrary = fc.bigIntN(256).filter((n:bigint) => n > 0n); 
const afterDeadlineWaits = fc.integer().filter((n: number) => n >= dl);
const beforeDeadlineWaits = fc.integer().filter((n: number) => n < dl);
```
- `fastCheck.bigIntN(256)` generates random 256bit integers either positive or negative
- `fastCheck.integer()` generates random integers either positive or negative

#### Units tests
```typescript
Deno.test("PT: Negative redeemer after deadline always succeeds", () => {
//          what fastcheck uses to get values
//              vvvv                                passes values to function
  fc.assert(fc.asyncProperty(                            //  vvvvvvvvv
    negativeBigIntArbitrary, afterDeadlineWaits, async (r: bigint,n: number) => {  // use our fastCheck value generators here
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        console.error('Test failed for r= '+ r +' with error: ' + error.message);
        throw error
      };
    }
  ),{numRuns: 100});                                                              // number of runs for fastCheck
});

Deno.test("PT: Positive redeemer after deadline always fails", () => {
  fc.assert(fc.asyncProperty(
    positiveBigIntArbitrary, afterDeadlineWaits,async (r:bigint, n: number) => {
      let errorThrown = false;
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        errorThrown = true;
      }
      assert(errorThrown,'Test failed for r= ' + r + ' and n= '+ n);      
    }
  ),{numRuns:100});
})

Deno.test("PT: Anything before the deadline always fails", () => {
  fc.assert(fc.asyncProperty(
    fc.bigIntN(256), beforeDeadlineWaits,async (r:bigint, n: number) => {
      let errorThrown = false;
      try {
        await runTest({deadline:BigInt(Date.now()+dl)},r,n);
      } catch (error) {
        errorThrown = true;
      }
      assert(errorThrown,'Test failed for r= ' + r + ' and n= ' + n);      
    }
  ),{numRuns:100});
})
```
- `fc.assert` specifies a test
- `fc.asyncProperty` is what is used to generate properties using custom value generators(filters) and pass them to a function which executes `runTest` in our example
- `{numRuns:100}` tells fastcheck how many runs to do
