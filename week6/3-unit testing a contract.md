# unit testing a contract
https://www.youtube.com/watch?v=vB8hyVq3HVo&list=PLNEK_Ejlx3x08fHgl_ZTlowVO8bjqITEh&index=3&ab_channel=IOGAcademy

This is the contract we will be testing:
```
newtype CustomDatum = MkCustomDatum { deadline :: POSIXTime }
unstableMakeIsData ''CustomDatum

{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatum -> Integer -> ScriptContext -> Bool
mkValidator (MkCustomDatum d) r ctx = traceIfFalse "expected a negative redeemer" $ r <= 0 &&
                                      traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        deadlineReached :: Bool
        deadlineReached = contains (from d) $ txInfoValidRange info
```
- it just checkes that a redeemer is negative, and that the unlocking tx is submitted after the deadline has passed

#### create some helped functions, we'll use later:
```
waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1000      <- time to wait in our example

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)  <- users to use in our example

valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator       <- creates a plutus validator from our validator
```
- in the above functions `onchcain` represents the module in which the validator has been writen
    - it's just importing the validator code

####  create 2 helper functions that manage the creation of the transactions
- the producing and the consuming transactions
```
use as deadline in datum     plutus type that we'll pass in when doing the transaction
               vv            vv 
lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val =
  mconcat                                  <-- using the monoid functionality of Value
    [ userSpend usp
    , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
    ]     ^^^                   ^^                ^^ 
        pays to script         hashing datum     wrapping our deadline in the datum type created at the top of this file
                          redeemer    user to pay   utxo to spend
                             vvv          vv            vvv
consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr ref val =
  mconcat
    [ spendScript valScript ref (mkI redeemer) (OnChain.MkCustomDatum dl)
    , payToKey usr val
    ]
```
- both of these functions return a `Tx` object
    - the `UserSpend`, `payToScript`, `spendScript` and `payToKey` functions, all return a `Tx` as well
        - `Tx` is a monoid, so we are using the monoid functionality to concat the transactions into one.
    - first the `userSpend` is used to create a transaction for a user, that causes him to spend a utxo
    - second the `payToScript` function is used to create a transaction that causes a script to receive a utxo
    - these 2 are then combined using `mconcat`, which creates a transaction that spends the users utxo and gives it to the script

## creating the script testing function

```
testScript :: POSIXTime -> Integer -> Run ()
testScript d r = do
  [u1, u2] <- setupUsers
  let val = adaValue 100
  sp <- spend u1 val                         <- spend returns our UserSpend
  submitTx u1 $ lockingTx d sp val              <- producer tx submitted
  waitUntil waitBeforeConsumingTx
  utxos <- utxoAt valScript                     <- get all utxo at validator
  let [(ref, out)] = utxos
  ct <- currentTimeRad 100                                  <- Time interval for TxInfoValidRange
  tx <- validateIn ct $ consumingTx d r u2 ref (txOutValue out)  <- second tx, with validRange and utxo to spend
  submitTx u2 tx                            
  [v1, v2] <- mapM valueAt [u1, u2]                     <- final balances of both users
  unless (v1 == adaValue 900 && v2 == adaValue 1100) $  <- Check if final balances match expected balances
    logError "Final balances are incorrect"
```
- the above code:
    1. sets up the users(2) 
    2. creates a locking Tx
    3. waits a timeframe specified in params
    4. creates a consuming transaction with a redeemer from the params
    5. validates mock state
- `spend` returns a `UserSpend` type, which is what we use in the `lockingTx` to get a transaction for that user
- `currentTimeRad 100` is not important but generates an Interval that starts 100ms before the current time and ends 100ms after the current time(Current time in center = time radius = 'Current Time Rad')
- `submitTx` submits a created tx in the mock state

## creating the actual test cases:
- the good/bad functions and general structure are explained in (2-plutus simple model.md)
```
main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing validator with some sensible values"
      [ good "User 1 locks and user 2 takes with R = -42 after dealine succeeds" $ testScript 50 (-42)
      , good "User 1 locks and user 2 takes with R = 0   after dealine succeeds" $ testScript 50 0
      , bad  "User 1 locks and user 2 takes with R = 42  after dealine fails   " $ testScript 50 42
      , bad  "User 1 locks and user 2 takes with R = -42 before dealine fails  " $ testScript 5000 (-42)
      , bad  "User 1 locks and user 2 takes with R = 0   before dealine fails  " $ testScript 5000 0
      , bad  "User 1 locks and user 2 takes with R = 42  before dealine fails  " $ testScript 5000 42
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 10_000_000) defaultBabbage
```
- this is just running the `testScript` function created in the previous step with different inputs, and testing whether they fail or succeed correctly
- the functionality of one of these tests is that:
1. the `testScript` will be executed
2. the `locked utxo` with datum `deadline` will be submitted
3. the testScript function will `wait` the given amount of time(first param)
4. the `consuming tx` will be submitted with the `redeemer` supplied. 
5. good and bad will catch if things run as expected. 
- the above is run once for each test in the `testGroup` list