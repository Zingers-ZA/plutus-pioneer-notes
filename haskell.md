# General:

- `$` gets rid of perenthesis. ie. anything that comes after it takes precedence over anything before it
    - `(+) 2 (2 + 4)` == `(+) 2 $ 2 + 4`

- `.` - the Dot operator
    - also results in less parenthesis but that is not the aim
    - it's function is to `chain functions`, it creates a single function from more than one function
    ```
    let second = head . tail
    second "asdf"
    -- 's'
    ```
- always use <b>lower case</b> for function and var names, because enums use <b>upper case</b>

### ghci

- typing `ghci` into terminal opens a repl
- use `:l <filename>` to load a local 'module' into the repl
- use `:r` to reload modules int he repl
- use `:t x` to get the type of something in ghci


### importing modules

```
import Data.List
import System.IO
```

# Types:

- statically typed
- haskel uses type inferance

- `Int` - 64 bit int
- `Integer` unbound int, can be as big as your memory can hold
- `Double` - decimals, precision up to 11 points
- `Bool` - true or false
- `Char` uses `'`
- `Tuple` usually 2 values of different types

<b>Declaring vars:</b>

```
always5 :: Int
always5 = 5
```

### Custom types:
- use `data` to define a custom type
```
    
       name         property types 
        vv       vvv vvvvvv vvvvvv vvvv
data Customer String String String Double
```
different syntax:
```
data Employee = Employee { 
                           name :: String,
                           position :: String,
                           id :: Int
                         }

data Dog = Dog { 
                 name :: String,
                 age :: Int
               } deriving (Eq, Show)
                   ^^^^^^^^^^^^^
         this syntax says to create instances of the type that implement the Eq and Show type classes
```

#### populating a type (creating an object, but not)
```
tomSmith :: Customer
tomSmith = Customer "Tom" "123 Main" 12.50

employeeSmith :: Employee
employeeSmith = Employee {name = "employee smith", position = "boss", id = 123}

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b      <-- strips out balance and returns
```

Custom data types with options (basically enum)
```
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```
- shape can be either Circle or Rectangle

### Type classes:
- groups for types
- eg. Num encloses `Int`, `Integer`, `Double` etc.

examples: `Num` 

# Functions

- functions must return something
- functions cannot start with an uppercase letter
- use `_` for ignored input params

do (allows chaining operations):
```
main = do
    putStrLn "ayo"
    name <- getLine
    putStrLn ("Hello " ++ name)
```
- the `<-` above runs the getLine function(gets input from terminal) and saves it as the name on the left
normal syntax:
```
--  arg1type  arg2type  returnType
--       vv      vv     vv
addme :: Int -> Int -> Int
addMe x y = x + y            < -- operation
--    ^^^
-- param names
```
without using type declaration
```
addMe x y = x + y
```
- uses type inference

#### syntax for <u>type classes</u>:
```
 shows that all "a's" should have an instance of the type class "Num"
         vvvv
addme :: Num a => a -> a -> a
addMe x y = x + y
```

#### functions can also overload
```
whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"
whatAge x = "Nothing Important"
```

### High order functions
- passing functions to other functions as if they were values
```
times4 :: Int -> Int
time4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]
                ^^^^^^^^^^^^^
                passing function to function(map)
```

`receive` a function is as follows:
```
-- (Int -> Int) says we expect a function that receives Int and returns an Int
doMult :: (Int -> Int) -> Int
doMult func = func 3
       ^^^^
       this is now the function we got in which takes an Int and returns an Int
```

### lambda functions 
- lambdas act like custom unnamed functions
- `(\x -> x + 1) 4` = `5 :: Integer`

```
dbl1To10 = map (\x -> x * 2) [1..10]
                ^^ 
                lambda function
```
the above code applies the lambda to each element in the range (1 to 10) through the use of map

# operators
- all operators are just functions
    - for example `(+) 1 2` == `1 + 2`
- `+ - / *`
- use `&&` and `||`
- use `not(True)` for !
- comparing ints for equality is done with `==` or `/=`
- `sum` sums ranges (eg. sum [0..1000])

there are pre-operators, eg:
`mod 5 4` == `5 % 4`
- backtick syntax can let you use these pre-operators in the normal place - eg ```5 `mod` 4```

# lists
- lists are singly linked
- can only add to the front(using cons)
- syntax: `nums = [3,2,1]`
- use `length x` to get length:  `lenNums = length [1,2,3]`
- concatting: `nums ++ [4,5,6]`
- <b>cons operator</b> one way to combine numbers into a list:
    - `nums = 2 : 6 : 4 : []` 
    - add 2 to list: `moreNums = 2 : nums`
- <b>Indexes</b>:
    - use `!!` eg: `[0,2,4] !! 1` == `2`
- <b>Contains</b>:
    - ```7 `elem` [1,2,7]``` == `true`
- repeat: `take 10 (repeat 2)`
- sort `sort [32,3,5,2]`
- filter: `filter (>2) [2,3,4]

# ranges:
- syntax is like `[0..1000]`
- even nums like `[2,4..20]`
- `letterList = ['A','C'..'Z']`
- infiniteList: `[10,20..]`
    - infinite list only generates up to what is required

- list comprehension:
```
myList = [x | x <- [0..500]]
--          ^^           
--         operator
myList2 = [x + y | x <- [0..500] | y <- [0..10]]
--          ^^           
--         operator
myList3 = [x | x <- [0..500], x * 3 <= 50]
--                             ^^^^^^^^
--                              filtering
```
- myList puts the right side of `|` into the left side

- myList3 has a filter after a `,`

#### tuples
- `bob = ("bob smith", 52)`  
- `fst bob` = "bob smith"
- `snd bob` = 52

# recursion
```
factorial :: Int -> Int
factorial 0 = 1                       <-- exit condition
factorial n = n * factorial (n - 1)
```

# Guards
- kinda like switch 
- pattern matching
```
isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
   ^^  ^^^^^^^^^^^^     ^^ 
 Guard   expression     output
```
`otherwise`:
```
isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True
```

# where
- uses `guards` to define switch of options
- `where` then is at the bottom and populates the vars that the guards use
```
batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "Terrible Batting Average"
	| avg <= 0.250 = "Average Player"
	| avg <= 0.280 = "Your doing pretty good"
	| otherwise = "You're a Superstar"
	where avg = hits / atBats 
      ^^^^^^
    vars defined here
```

# if
```
doubleEvenOnly y = 
    if (y `mod` 2 /= 0)
        then y
        else y * 2
```


# case

```
getClass :: Int -> String
getClass n = case n of
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go some place else"
```
similar to guards except used for values instead of patterns


# enumerations
- list of possible types
```
                      enums denoted with |
                     vvvvvvvvvvvvvvvvv
data BaseballPlayer = Pitcher | Catcher

-- can also be on different lines
data BaseballPlayer = Pitcher 
                    | Catcher
                    | Infielder
```

### comments: 
```
-- comment
{-
comment
-}
```

# Compiling
`ghc --make <name of file>`
- name of file in current dir
- don't include .hs