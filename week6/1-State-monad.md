# The state monad
https://www.youtube.com/watch?v=8tWzG0ML6Z4&list=PLNEK_Ejlx3x08fHgl_ZTlowVO8bjqITEh&index=1&ab_channel=IOGAcademy

- State monad is used extensively for testing plutus code

## illustrative example without monad:

- we'll create some new types, 
    1. a utxo 
    2. a 'blockchain'(list of Utxo's)
    3. an initial blockchain state

```
-- Mock UTxO type
data UTxO = UTxO { owner :: String , value :: Integer }
    deriving (Show, Eq)

-- Mock blockchain type
newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

-- Initial blockchain state
initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]
```
- then we'll create some functions to run a test 
- the below function takes in some 'addresses', and values(utxos) to send, as well as a State of the 'blockchain'
    - it returns the state after the sending has occured

```
sendValue :: String -> Integer -> String -> Mock -> (Bool, Mock)
sendValue from amount to mockS =
    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    in if totalSenderFunds >= amount
        then (True, Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos)
        else (False, mockS)
```

- now we'll define a function that uses our `sendvalue` function
- the idea here is to do some testing
```
multipleTx :: (Bool, Mock)
multipleTx =
    let (isOk,  mockS1) = sendValue "Alice" 100 "Bob"   initialMockS  <-- we are having to pass in the state to each call
        (isOk2, mockS2) = sendValue "Alice" 300 "Bob"   mockS1        <-- here
        (isOk3, mockS3) = sendValue "Bob"   200 "Rick"  mockS2        <-- and here 
    in (isOk && isOk2 && isOk3, mockS3)                   ^^
```
- the above is error prone because we are manually maintaining the state of our mocked blockchain, and having to pass it in to have it updated and tested.

## Example using the State monad

- once again we'll create our types:
```
-- Mock UTxO type
data UTxO = UTxO { owner :: String , value :: Integer }
    deriving (Show, Eq)

-- Mock blockchain type
newtype Mock = Mock { utxos :: [UTxO] }
    deriving (Show, Eq)

-- Initial blockchain state
initialMockS :: Mock
initialMockS = Mock [ UTxO "Alice" 1000 ]
```
- next we will re-write our `sendValue` function to use the State monad
```
-- function signature for reference
-- newtype State s a = State { runState :: s -> (a, s) }
                               ^^^ --------------------------important, exposes get, put, state

sendValue' :: String -> Integer -> String -> State Mock Bool  <-- now returns a State type
sendValue' from amount to = do
    mockS <- get                 
             ^^^-------------------------------------------- exposed by runstate, gets current state value

    let senderUtxos = filter ((== from) . owner) (utxos mockS)
        blockchainWithoutSenderUtxos = filter ((/= from) . owner) (utxos mockS)
        totalSenderFunds = sum (map value senderUtxos)
        receiverUtxo = UTxO to amount
        senderChange = UTxO from (totalSenderFunds - amount)
    if totalSenderFunds >= amount
        then do
            put $ Mock $ [receiverUtxo] ++ [senderChange] ++ blockchainWithoutSenderUtxos
            ^^^ ----------------------------------------------------- also exposed by runState, updates current State
            return True
        else return False
```
- our function is now updating and managing some state on it own, by retrieving the current state at the start with `get`, and then updating it at the end with `put`
- `State` takes in an execution step(`s`) and an initial state(`a`), and uses `runState` to execute the execution step and return the updated state
- now update the multipleTx function which is actually running our tests:
```
multipleTx' :: (Bool, Mock)
multipleTx' = runState (do                      <-- 
    isOk  <- sendValue' "Alice" 100 "Bob"       <-- no more need to pass in state manually
    isOk2 <- sendValue' "Alice" 300 "Bob"
    isOk3 <- sendValue' "Bob"   200 "Rick"
    return (isOk && isOk2 && isOk3))
    initialMockS

```
- in the above example the `do` block is the execution step that `runState` will execute, and the `initialMockS` is the initial value of the state
- since the `sendValue'` function is using `get` and `put` to manage state, we can just call the function and it's state will be fed in and managed by the State monad as needed