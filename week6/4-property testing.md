# Property testing
https://www.youtube.com/watch?v=pF8HpKmaQi4&list=PLNEK_Ejlx3x08fHgl_ZTlowVO8bjqITEh&index=4&ab_channel=IOGAcademy

- unit testing is good, but it may not test enough variations of input to confidently say the tests cover all cases
- property testing smart contracts refers to simulating 1000's of tests to increase confidence for all variations of input
- quickCheck is a library that can check 1000's of simulated properties for tests
    - quickCheck is not specific to plutus, it's just a haskell library

#### setting up the main test, with test cases
```haskell
main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing script properties"
      [ testProperty "Anything before the deadline always fails       " prop_Before_Fails
      , testProperty "Positive redeemer after deadline always fails   " prop_PositiveAfter_Fails
      , testProperty "Negative redeemer after deadline always succeeds" prop_NegativeAfter_Succeeds
      ]
```
- 3 tests

#### Helper functions :

```haskell
-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp $ initMock defaultBabbage (adaValue 10_000_000) in property a

-- Make POSIXTime an instance of Arbitrary so QuickCheck can generate random values to test
instance Arbitrary POSIXTime where
  arbitrary = do
    n <- choose (0, 2000)
    return (POSIXTime n)
```
- the first function this is somewhat boilerplate, just creating a `Testable` instance for our previous `Run a` to allow it to work with QuickCheck
- the `Abritrary` instance is used to wrap POSIXTime because it is not a standard type that QuickCheck can use
    - we just have to give some constraints so Quickcheck knows what types of values to pass
    - `choose` is used for this

#### Testing properties
```haskell
-- All redeemers fail before deadline 
prop_Before_Fails :: POSIXTime -> Integer -> Property
prop_Before_Fails d r = (d > 1001) ==> runChecks False d r
--                       ^^^^^^
--                  setting the constraints for deadline

-- Positive redeemer always fail after deadline
prop_PositiveAfter_Fails :: POSIXTime -> Integer -> Property
prop_PositiveAfter_Fails d r = (r > 0 && d < 999) ==> runChecks False d r
--                                                      ^^
--                                                  function to do checking(defined below)

-- Negative redeemers always succeed after deadline
prop_NegativeAfter_Succeeds :: POSIXTime -> Integer -> Property
prop_NegativeAfter_Succeeds d r = (r < 0 && d < 999) ==> runChecks True d r
--                                                                  ^^^^
--                                                              params to test(False means should fail as per our own logic, see below)
```
- these are the 3 tests which are used in our `main` function

#### runChecks function
```haskell
-- | Check that the expected and real balances match after using the validator with different redeemers
runChecks :: Bool -> POSIXTime -> Integer -> Property
runChecks shouldConsume deadline redeemer = 
  collect (redeemer, getPOSIXTime deadline) $ monadic property check
    where check = do
            balancesMatch <- run $ testValues shouldConsume deadline redeemer
            assert balancesMatch
```
- `runChecks` will be executed for lots of variations in input that quickCheck can come up with, within the constraints defined in the 3 functions we created above
- `collect` is just collecting some info about the tests, for debugging later, not very important
- `monadic property` is boilerplate
- `check` is the important function, it's `run`ning the `testValues` function(defined below) with the params supplied to it
    - these params are supplied by the 3 property testing functions defined above
- the `assert`ion will make sure that the balances returned by the testValues function match at the end

#### testValues function

This function is simulating the transactions that are taking place in our test. Very similar to the one described in (3-Unit testing a contract.md)

```haskell
-- Function to test if both creating an consuming script UTxOs works properly
testValues :: Bool -> POSIXTime -> Integer -> Run Bool
testValues shouldConsume datum redeemer = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXOs that we should spend
  submitTx u1 $ lockingTx datum sp val      -- User 1 submits "lockingTx" transaction
  -- WAIT FOR A BIT
  waitUntil waitBeforeConsumingTx
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(oRef, oOut)] = utxos                -- We know there is only one UTXO (the one we created before)
      tx = consumingTx datum redeemer u2 oRef (txOutValue oOut)            -- Define transaction to be submitted
      v2Expected = if shouldConsume then adaValue 1100 else adaValue 1000  -- Define expected balance for user 2
  ct  <- currentTimeRad 100                 -- Create time interval with equal radius around current time
  tx' <- validateIn ct tx                   -- Build final consuming Tx
  if shouldConsume then submitTx u2 tx' else mustFail . submitTx u2 $ tx'  -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]               -- Get final balances
  return $ v1 == adaValue 900 && v2 == v2Expected -- Check if final balances match expected balances
```

# full test case for replication:

```haskell
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified NegativeRTimed       as OnChain
import           Control.Monad           (mapM, replicateM)
import           Plutus.Model            (Ada (Lovelace), DatumMode (HashDatum),
                                          Run, Tx,
                                          TypedValidator (TypedValidator),
                                          UserSpend, ada, adaValue,
                                          defaultBabbage, initMock, mustFail,
                                          newUser, payToKey, payToScript,
                                          runMock, spend, spendScript, submitTx,
                                          toV2, userSpend, utxoAt, valueAt, waitUntil, currentTimeRad, validateIn)
import           Plutus.V2.Ledger.Api    (PubKeyHash, TxOut (txOutValue),
                                          TxOutRef, Value, POSIXTime (POSIXTime, getPOSIXTime))
import           PlutusTx.Builtins       (Integer, mkI)
import           PlutusTx.Prelude        (Bool (..), Eq ((==)),
                                          return, ($), (&&), (.))
import           Prelude                 (IO, Ord ((<), (>)),
                                          mconcat)
import           Test.QuickCheck         (Property, Testable (property),
                                          collect, (==>), Arbitrary (arbitrary), choose)
import           Test.QuickCheck.Monadic (assert, monadic, run)
import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.QuickCheck   as QC (testProperty)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

-- | Test the validator script
main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing script properties"
      [ testProperty "Anything before the deadline always fails       " prop_Before_Fails
      , testProperty "Positive redeemer after deadline always fails   " prop_PositiveAfter_Fails
      , testProperty "Negative redeemer after deadline always succeeds" prop_NegativeAfter_Succeeds
      ]

---------------------------------------------------------------------------------------------------
-------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------

-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp $ initMock defaultBabbage (adaValue 10_000_000) in property a

-- Make POSIXTime an instance of Arbitrary so QuickCheck can generate random values to test
instance Arbitrary POSIXTime where
  arbitrary = do
    n <- choose (0, 2000)
    return (POSIXTime n)

-- Time to wait before consumming UTxO from script
waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1000

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 1000)

-- Validator's script
valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator

-- Create transaction that spends "usp" to lock "val" in "giftScript"
lockingTx :: POSIXTime -> UserSpend -> Value -> Tx
lockingTx dl usp val =
  mconcat
    [ userSpend usp
    , payToScript valScript (HashDatum (OnChain.MkCustomDatum dl)) val
    ]

-- Create transaction that spends "giftRef" to unlock "giftVal" from the "valScript" validator
consumingTx :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dl redeemer usr ref val =
  mconcat
    [ spendScript valScript ref (mkI redeemer) (OnChain.MkCustomDatum dl)
    , payToKey usr val
    ]


---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------


-- All redeemers fail before deadline 
prop_Before_Fails :: POSIXTime -> Integer -> Property
prop_Before_Fails d r = (d > 1001) ==> runChecks False d r

-- Positive redeemer always fail after deadline
prop_PositiveAfter_Fails :: POSIXTime -> Integer -> Property
prop_PositiveAfter_Fails d r = (r > 0 && d < 999) ==> runChecks False d r

-- Negative redeemers always succeed after deadline
prop_NegativeAfter_Succeeds :: POSIXTime -> Integer -> Property
prop_NegativeAfter_Succeeds d r = (r < 0 && d < 999) ==> runChecks True d r


---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------


-- | Check that the expected and real balances match after using the validator with different redeemers
runChecks :: Bool -> POSIXTime -> Integer -> Property
runChecks shouldConsume deadline redeemer = 
  collect (redeemer, getPOSIXTime deadline) $ monadic property check
    where check = do
            balancesMatch <- run $ testValues shouldConsume deadline redeemer
            assert balancesMatch


-- Function to test if both creating an consuming script UTxOs works properly
testValues :: Bool -> POSIXTime -> Integer -> Run Bool
testValues shouldConsume datum redeemer = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 ADA ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXOs that we should spend
  submitTx u1 $ lockingTx datum sp val      -- User 1 submits "lockingTx" transaction
  -- WAIT FOR A BIT
  waitUntil waitBeforeConsumingTx
  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(oRef, oOut)] = utxos                -- We know there is only one UTXO (the one we created before)
      tx = consumingTx datum redeemer u2 oRef (txOutValue oOut)            -- Define transaction to be submitted
      v2Expected = if shouldConsume then adaValue 1100 else adaValue 1000  -- Define expected balance for user 2
  ct  <- currentTimeRad 100                 -- Create time interval with equal radius around current time
  tx' <- validateIn ct tx                   -- Build final consuming Tx
  if shouldConsume then submitTx u2 tx' else mustFail . submitTx u2 $ tx'  -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- mapM valueAt [u1, u2]               -- Get final balances
  return $ v1 == adaValue 900 && v2 == v2Expected -- Check if final balances match expected balances
```