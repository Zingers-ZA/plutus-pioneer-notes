# Plutus simple model testing
https://www.youtube.com/watch?v=Sft02LeXA_U&list=PLNEK_Ejlx3x08fHgl_ZTlowVO8bjqITEh&index=2&ab_channel=IOGAcademy

We're going to define a main function to do some testing
```
main :: IO ()
main = defaultMain $ do 
        testGroup
            "Test simple user transactions"
            [
                good "simple spend" simpleSpend,
                bad "not enough funds" notEnoughFunds
            ]
            where 
                bad msg = good msg . mustFail
                good = testNoErrors (adaValue 10_000_000) defaultBabbage
```
in the above: 
- `testGroup` allows us to group a list of tests together
- each function in the tests should be of type `Run a`
    - for example, simpleSpend is a `Run a`
- `good` is a wrapper around `testNoErrors`, which makes sure that there is no error thrown during execution of a test
- `testNoErrors` is a Plutus function which accepts `Value -> MockConfig -> String -> Run a`
    - in this case, because good is wrapping it, the params from good are passed down to it, so it gets `"simple spend"` as the string and `simpleSpend` as the `Run a`
    - it also takes in the initial value of ada and assigns it to some admin mock user, and mocks the blockchain using defaultBabbage
- `defaultBabbage` is the a default Mock Config, and represents some default state for the mock blockchain
- `mustFail` is a Plutus function  and returns an error if things succeed, and no error if things fail

#### Now lets see the actual tests

first we need to define a helper function
- this just sets up some users to use for transactions
```
setUpUsers :: Run [PubKeyHash]
setUpUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)
                 ^^           ^^
            do 3 times        function fro   m Plutus.model.contract  
```
- this function creates 3 new users
- `newUser` is a plutus function that takes in a value and returns a 'user'(pub key hash) with a utxo with that amount of funds.
    - it gets these funds from the 'admin' user that was created by `testNoError` when it was initially setup using (ada 10_000_000) and defaultBabbage

#### simpleSpend test
```
simpleSpend :: Run Bool
simpleSpend = do 
    users <- setupUsers                <-- creates the 3 users from setupUsers
    let [u1, u2, u3] = users
    sendValue u1 (adaValue 100) u2     <-- sending
    sendValue u2 (adaValue 100) u3
    isOk <- noErrors
    vals <- mapM valueAt users
    return $ isOk &&
            (vals == fmap adaValue [900, 1000, 1100])
```
- `sendValue` is a plutus function that looks like `PubKeyHash -> Value -> PubKeyHash -> Run ()`
    - it's taking a user to get from, and send to, and transferring funds
    - sendValue is maintaining the state monad in the background
- `noErrors` is a plutus function that checks the state that the previous test steps have generated and verifies that no errors occurred during that process
    - in the above code it's just verifying that all the transactions succeeded
- `valueAt` is a plutus function that take in a pubkeyhash and returns the value at that address (in the mock)
    - in the above example we need to map this function to each user 
- `return` must be used to wrap the boolean that is returned as a `Run Bool`, since that is the return type of the function

#### notEnoughFunds test

```
notEnoughFunds :: Run Bool
notEnoughFunds = do
    users <- setupUsers
    let [u1, u2, u3] = userse
    sendValue u1 (adaValue 10000) u2   <-- sending too many tokens(user doesn't have funds)
    noErrors                           <-- running this should retrieve the error that will have occurred from that preceding transaction 
```
- because `noErrors` is a Plutus.model.contract function and used in tests, it already returns a `Run Bool`, and since the result of the last line in a `do` block is returned by default, there is no need to write a return call for this function
    - it would have looked like 
        ```
        isOk <- noErrors
        return isOk
        ```