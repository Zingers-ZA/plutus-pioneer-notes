# A stablecoin contract project

The following project will implement an over-collateralized algorithmic stablecoin

## Approach:
- `collateral` validator
    - locks and releases collateral
- `minting` validator
    - to control the minting and burning of stablecoins
- the value of the collateral must exceed the value of the coins minted
    - this incentivizes liquidations, if the collateral you provide in exchange for your stablecoins becomes less than a predefined threshold(eg 150%), someone can liquidate you and get the extra collateral. This way you are forced to always provide enough collateral in exchange for your stablecoins
- `price oracle`
    - the price oracle will be used to track the price of the currency we want to peg our stablecoin to
    - the approach is to create a minting policy and include nfts from that policy in utxos that contain datums with the data we need. 
        - the nft is required because any one can send utxos to an address, so we need some way to know that the utxo came from the oracle owner.
        - this will be the nft from a policy which only allows minting from a certain party

## Deploying approach
- `oracle` mint NFT's and include data in datums of utxos sent to some address
    - in this case the oracle utxos will sit at an oracle address which only allows spending of the utxos from a certain party. 
    - ensures that only the policy owner can update the oracle utxo
- `minting` and `collateral` validators
    - these will be included in transactions with utxos at an always false validator, which will mean that they exist forever, and the attached scripts can be used as reference scripts

## Use case:

### Minting stablecoins:

- Include the minting policy as reference script
    - this will verify that the correct amount of collateral has been paid
- Include the oracle utxo as a reference input
- Send the correct amount of collateral to the collateral validator address
    - this utxo contains a datum with :
        - the owner of the collateral
        - the amount of stablecoins minted
- Send the correct amount of stablecoins to your own address

### Burning stablecoins

- Include the minting policy as reference script(because we are burning)
- Include the collateral validator as reference script
    - to verfiy the collateral unlock
- Retrieve the correct amount of collateral from the collateral validator
- Provide the correct amount of stablecoins to burn
- Send a utxo with your collateral to yourself

### Liquidating someones position

- this is similar to burning stable coins except it allows you to burn your own stablecoins to unlock someone elses collateral
    - this only becomes possible when the value of the locked collateral falls below 150%(in our case) of the value of the minted stablecoins at the price they were minted at
    - it's done to incentivize always having the correct amount of ada locked, because as soon as a collateral utxo that was created at a certain price, falls below the threshold, someone else can mint their own stablecoins(with the correct amount of collateral for the new price), and consume the (now) incorrect collateral that was previously locked. There is a slight difference in price here which the new buyer gains as profit. 

- Spend your own stablecoins 
    - must be the same amount of stablecoins that were minted by the collateral you are trying to retrieve
- Include stablecoin policy as reference script
    - since we are buring the token
- Include the collateral validator since we need to spend collateral
- Include the oracle utxo as reference input
- Send the collateral to yourself

### Retiring the service

- the developer wants to call it quits

- Spend the oracle utxo and don't update it with a new one
    - users can't mint new stablecoins because the oracle utxo is required for that
    - users can't collect other users collateral because the oracle utxo is required aswell
    - users can still retrieve their own collateral because the oracle isn't required for that


