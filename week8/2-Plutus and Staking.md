- Up until now only seen a `ScriptPurpose` of either `Minting` or `Spending`
- In this lecture will explore `Rewarding`
```haskell
Minting CurrencySymbol
Spending TxOutRef
Rewarding StatkingCredential
Certifying DCert
```


#### Rewarding script purpose
```haskell
Rewarding StatkingCredential
```

Recall cardano address structure:
```
header + payment part + staking part
```
When creating a transaction that withdraws staking rewards that a staking address has earned, the transaction must be signed by that staking address. A typical transaction will include a signature from the payment part of the address(because at least one utxo will need to be spent for the transaction to be valid), as well as a signature from the staking part of the address, to ensure that the address entitled to the staking rewards is doing the withdrawing
- similar to the payment part, the staking part is either:
1. the hash of a `staking pubkey`
    - in which case the signature of that address must be included to validate the transaction
2. the hash of a `staking policy`
    - in which case the corresponding reward policy is executed to validate the transaction
 
this means you can have a script staking address, so rewards will accumulate at a script address and transactions can be submitted to withdraw that reward. The reward script will be provided and that will be executed to validate the transaction

#### Certifying script purpose

There are various certifications that can be attached to a transaction
```haskell
Certifying DCert
```
Relevant to staking:
- `Registration` - when a new stake address is created(during normal wallet creation - the stake part) the staking address must first be registered
    - the transaction must contain a registration certificate for that  
    - will pay a deposit
- `Delegation` - if you want to delegate or change delegation to a pool, you must attach a delegation cert to the transaction
- `De-registration` - if you want to deregister a staking address, you must attach a de-registration certificate to the transaction
    - will get back deposit from registering

The `Certifying` script purpose is used when a transaction is submitted with a certificate attached and can decide whether those actions are legal or not

## Staking in txInfo

Relevant txInfo for 
```haskell
TxInfo
--  ...
    txInfoDCert :: [DCert]
    txInfoWdrl :: Map StakingCredential Integer
--  ...
```
`txInfoWdrl` - 'TxInfoWithdrawl' has information about the withdrawl given within a certain transaction  
This `Map` will have all the details of the withdrawls of the current transaction
- in most cases will only be one key with a staking credential of the current script

## Example of a plutus staking script

```haskell
{-# INLINABLE mkStakeValidator #-}
mkStakeValidator :: Address -> () -> ScriptContext -> Bool
mkStakeValidator addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True          
--                     ^^
--              allows all certifying transactions
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = case PlutusTx.lookup cred $ txInfoWdrl info of
        Just amt -> amt
        Nothing  -> traceError "withdrawal not found"

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
--                   ^^
--                  loop f with params(f is below function)
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + valueOf (txOutValue o) adaSymbol adaToken
            | otherwise              = n

{-# INLINABLE mkWrappedStakeValidator #-}
mkWrappedStakeValidator :: Address -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator = wrapStakeValidator . mkStakeValidator

stakeValidator :: Address -> StakeValidator
stakeValidator addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| mkWrappedStakeValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
```
- staking validators have the same structure as minting policies 
    - `Redeemer -> ScriptContext -> ()`
- in this case there is a parameter which is of type address 
- the script verifies that the transaction pays half of the rewards to a given address in the parameter
- `PlutusTx.lookup` allows finding keys in a map
    - in this case looking up staking credential in the txInfoWdrl
- `foldl` is used to loop over all the outputs and sum up the total paid to the address given
    - the function `f` is applied to each element inside `txInfoOutputs`

#### Converting a bech32 string to a plutus `Address`
```haskell

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                                       =
    Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (Api.SlotNo x) (TxIx y) (CertIx z))) =
    Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                                           =
    Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case Api.deserialiseAddress Api.AsAddressAny $ pack x of
    Nothing                                          -> Nothing
    Just (Api.AddressByron _)                        -> Nothing
    Just (Api.AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }


-- usage
getAddr :: String -> IO ()
getAddr bech32 = do
    case tryReadAddress bech32 of
        Nothing   -> -- ...
        Just addr -> -- ...
```