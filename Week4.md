# Week 04 - Emulator Trace and Contract Monads

## Introduction:

- Note: Plutus script is used to validate the minting and burning of native tokens
- Going to chat about offchain in this lecture
- Offchain code doesn't get compiled to plutus script
    - but it does use more sophisticated concepts(eg. monads)

## 1. Monads
#### Start with IO:
Example with mainstream langauge like Java:
```
public static int foo() {
}
.. foo()
... foo()
```
- It's possible that the two different method calls result in different outputs(example: it could ask the user for input)

With Haskell:
```
foo :: Int
foo = ...
... foo ... foo
```
- foo will always have the same value
    - not the same in a program like java because there are side effects in those languages

#### IO ()
```
foo :: IO Int
foo = ...
```
- IO is a type constructor, implemented in the language itself
- <b>IO means that the following method is a recipe to compute an int and that recipe can contain side effects</b>
- `IO ()` acts like a main method in java

#### Hello world in haskell:
```
main :: IO ()
main :: putStrLn "Hello, world!"
```
- Return is unit, no interesting result. Only interested in side effects. 
- Can compile IO functions (or any main function) with `cabal run <file>`

#### Functor:
`fmap :: (a -> b) -> f a -> f b`
- converts `f a` into `f b`
- does so by applying the given function `(a -> b)` to `f a`
- eg:
```
toUpper 'q' = 'Q'
map toUpper "Haskell" = "HASKEL""
fmap (map toUpper) getLine = "waits for user input, then applies map toUpper to that string"
```

`>>` chains two IO actions together ignoring the output of the first  
putStrLn "Hello" >> putStrLn "World"  
Hello  
World  

`>>=` is called Bind  
`(>>=) :: Monad m => m a -=> (a -> m b) -> m b`
- does not ignore the result of the first call
- if there is a function which returns an `a`(`m a`), and there is a function which given an `a` returns a `b`(`(a -> m b)`) then those two can be combined to a recipe that creates a `b` (`m b`)
- `getLine >>= putStrLn`: getLine produces an `IO String`, which is then passed to putStrLn which takes an `IO String` and produces an `IO ()`


`return`  
`return :: Monad m => a -> m a`
- recipes that return immediately before executing side effects

### The Maybe Type:
`type Maybe :: * -> *`
`data Maybe a = Nothing | Just a`
- optional a
- `maybe a` can either evaluate to `Just a` or `nothing`

### The Either Type:
`type Either :: * -> * -> *`  
`data Either a b = Left a | Right b`
- type to contain either one type or another type  
eg:   
`Left "Haskell" :: Either String Int` returns `Left "Haskell`  
`Right 7 :: Either String Int` returns `Right 7`  

### Writer Example:

``` 
data Writer a = Writer a [String]
    deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["Number: " ++ show n]

tell :: [String] -> Writer ()
tell = Writer ()

foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo (Writer k xs) (Writer l ys) (Writer m zs)
    let 
        s = k + l + m 
        Writer _ us = tell ["sum: " ++ show s]
    in
        Writer s $ xs ++ ys ++ zs ++ us
```

## Monads explanation:
```
type Monad :: (* -> *) -> Constraint
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```
- Lars: "A Monad is some sort of computation with a side effect" 
- Concept of computation with some additional side effects
    - real world
    - log messages
    - failure
- Possibility to bind multiple of these such(above) computations together
- Each monad comes with it's own bind that tells you how you can combine two computations
- Whats important to understand when working with a monad is that 1. you can chain multiple computations together and 2. what specific functionality that monad has. 
- Also possible to have no side effects
    - return 
    - Just
    - Either Right 
- Can use do notation in monads
- Note for future: sometime it can be desireable to have two side effects(logging, and failue), in which case one of the approaches is called Monad transformers, where you can build custom Monads using these transformera
- Uses type constructor m
- Monad has a super class - `Applicative`:
```
type Applicative :: (* -> *) -> Constraint
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
- pure is similar to return
- <*> (pronounced ap) not important yet

### Standard way to implement a monad:

There is a general pattern, if you want to define a monad you always define return and bind and just to make the compiler happy and also to give instances for functor and applicative, theres always a standard way of doing that.  

Given this, the standard way to implement functor and applicative is to use helper functions from `Control.Monad` (liftM, pure, ap)
```
instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
```
- The above is the standard pattern if you implement a monad
    - implement return and bind for the Monad
    - then for the Functor and Applicative you just use the helper functions from `Control.Monad`

```
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts = mx my mz = 
    mx >>= \k -> 
    my >>= \l ->
    mz >>= \m ->
    let  s = k + l + m in return s
```

#### do Notation

- Slightly less noisy way to write the same function as above:
- Whenever you want to give a result a name(eg k) do notation makes it a bit more readable
```
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts = mx my mz = do
    k <- mx
    l <- my
    m <- mz
    return $ k + l + m
```
- can also use the let keyword inside `do`, in which case you don't need the `in`
```
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts = mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
```
#### Other Notes:
- Contract is a monad(used in offchain code)

## 2. The EmulatorTrace Monad

- Faster way to test contract code instead of using the plutus playground
- Similar to what you do manually in the simulate part of playground(build the tx flow)
- Defined in Plutus.Trace.Emulator in package Plutus-contract
- Note that EmulatorConfig is an instance of the Default class so you can use the def constructor with it. eg ` def :: EmulatorConfig`
- Most general way to run them is using runEmulatorTrace:  
`runEmulatorTrace :: EmulatorConfig -> EmulatorTrace () -> ([EmumlatorEvent], Maybe EmulatorErr, EmulatorState)`
- The above is a pure function, no side effects
    - EmulatorConfig:
```
EmulatorConfig
    _initialChainState :: InitialChainState
    _slotConfig :: SlotConfig
    _feeConfig :: FeeConfig

InitialChainState = Either InitialDistribution TxPool
    InitialDistribution = Map Wallet Value
    TxPool = [Tx]

SlotConfig
    scSlotLength :: Integer          - Length (number of milliseconds) of one slot
    scSlotZeroTime :: POSIXTIME      - Beginning of slot 0 (in milliseconds)

FeeConfig
    fcConstantFee :: Ada             - Constant fee per transaction in lovelace
    fcScriptsFeeFactor :: Double     - Factor by which to multiply the size-dependant scripts fee
```

### Running an EmulatorTrace:

- Most trivial case emulator, just returns unit: `runEmulatorTrace def $ return ()` (in repl)
    - Outputs a bunch of arbitrary data to the console.
- `runEmulatorTraceIO` 
    - output is more readable
    - uses default config
    - runs an IO
    - eg. `runEmulatorTraceIO $ return ()`
- using `runEumlatorTraceIO'` allows you to supply a config:  
```
runEmulatorTraceIO' :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()

TraceConfig :: (Wallet.Emulator.MultiAgent.EmulatorEvent' -> Maybe String) -> GHC.IO.Handle.Types.Handle -> TraceConfig
    - First arg allows you to filter output events by string(can see it returns maybe string)
    - Second arg allows you to write to a hanlde(file or console, etc)
```

#### Example 1:
```
test :: IO ()                                                            - test function type
test = runEmulatorTraceIO myTrace                                        - the part that executes the below function

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints               - store wallet in something called a handle
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams                                 - call endpoint
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2    - get the mock wallet pubkeyhash
        , gpDeadline    = slotToBeginPOSIXTime def 20                    - get the posixtime
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20                                              - wait for slot 20
    callEndpoint @"grab" h2 ()                                           - call second endpoint
    s <- waitNSlots 2                                                    - wait 2 slots
    Extras.logInfo $ "reached " ++ show s                                - logs output
```
- Notes:
    - `Extras.logInfo` comes from `import Control.Monad.Freer.Extras as Extras`
    - `@"give"` is called a type activation/type level string (activated by including `{-# LANGUAGE TypeApplications #-}`) and acts as the type of the `endpoint`  
    - `void` comes from `Data.Functor` and ignores the result of `waitUntilSlot 20`
    - Log output from contract(eg. traceIfFalse) is marked by "CONTRACT LOG"

## 3. The contract monad:

`Contract w s e a`

`w` allows the contract to write log messages of type 'w'  
    - purpose is not 100% to log but more to pass information to other contracts    
`s` specfies the endpoints schemas  
`e` is the type of error messages  
`a` the output type

### Discussing `e` (Error):
#### Example 1:
```
myContract1 :: Contract () Empty Text ()                                 - in format 'Contract w s e a'
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1
```
- Notes
    - `Contract.logInfo`, qualifying the logInfo with Contract tells the compiler to log from the contract instead of the Emulator which also has the same function
    - `@String` tells the Contract.logInfo function to use string as the type might be ambiguous
    - `Empty` is a type you can use to specify no endpoints for `s`
    - `Text` is a more efficient type than string
    - as in the previous example, `void` is digarding the error from `Contract.throwError`

#### Example 2 (handling an error):

```
myContract2 :: Contract () Empty Void ()                         - this contract cannot throw an exception(because there is no void type)
myContract2 = Contract.handleError 
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1                                                  - note this handler catches the error thrown in example 1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract1

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace1
```
- Note the `handleError` function looks like this: `handleError ::
  (e -> Contract w s e' a) -> Contract w s e a -> Contract w s e' a`
    - It executes the second argument, and if there is no exception `a` will be the overall output
    - If there is an exception, then the first argument(handler) is applied to it with `e` as the input
- `unpack` is used like `show` but for the Text type

### Discussing `s` (The Schema)
#### Example 1:

```
type MySchema = Endpoint "foo" Int                      - 'foo' is a type level string(made possible by importing {#- LAN.. DataKinds -#})

myContract3 :: Contract () MySchema Text ()
myContract3 = 
    awaitPromise $ endpoint @"foo" Contract.logInfo     - awaitPromise breaks execution until the promise is returned from endpoint

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (knownWallet 1) myContract3      - binding the output to h1
    callEndpoint @"foo" h 42
```
Notes:
- `endpoint :: ....  => (a -> Contract w s e b) -> Promise w s e b`
- Promise is a contract that is blocked, waiting to be triggered by outside stimulus
- The first argument is a function that takes an input of type `a` and turns it into a contract
- The behaviour of this is that contract3 blocks on the promise, until it receives an input of type `@"foo"`(Int), in which case it executes the Contract supplied after it. In this trivial case it is `Contract.logInfo`
- `awaitPromise` waits for a promise to be returned and then creates a contract from that

#### Example 2 - More than one endpoint:

```
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String          - add extra endpoint

myContract3 :: Contract () MySchema Text ()
myContract3 = 
    awaitPromise $ endpoint @"foo" Contract.logInfo
    awaitPromise $ endpoint @"bar" Contract.logInfo                   - add extra endpoint

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (knownWallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"                                   - call extra endpoint
```
Notes: 
- `.\/` is called a type operator, it is a type constructor like Maybe or List or Either(`import {-# LAN.. TypeOperators #-}`)

### Discussing `w` (The Writer) 
- `w` cant be an arbitrary type, it must be an instance of `monoid`(very common class in haskell, list implements it)
```
type Monoid :: * -> Constraint
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```
- Mempty is a type constructor for an enpty version of type `a` (example `mempty :: [Int] = []`)
- Mappend is a type constructor to append things of similar types (example `mappend [1 :: Int] [2] = [1,2]`)
    - this allows the `w` to start off in a mempty state, and then mappend as it needs to 

#### Example 1:
```
myContract4 :: Contract [Int] Empty Text ()
myContract4 :: do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
```
Notes: 
- When inside a contract function wait slots is done with `Contract.waitNSlots` but inside the emulator it's done with `Emulator.waitNSlots`
- Way we can look up the state of a running contract is by using the `observableState`
    - as an argument it takes a handle, this should be the handle of the contract we want to observe
    - lastly you can bind that output to a var.
- Using `tell` writes to whatever the `w` is.


## 4. HomeWork:











