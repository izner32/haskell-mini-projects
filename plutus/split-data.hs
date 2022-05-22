{-
goal: lock some ada in a script output and splits them evenly between two recipient 

- declare the split app on how it's gonna look like 
    - declare the datatype of the main function - splitData
- defining the validator script
    - this is the onchain(occurs in blockchain while offchain occurs outside the blockchain) part of the script/code 
- asking for input
- locking the funds 
- unlocking the funds 

-}

-- 1. import necessary library 
import Control.Monad (void) -- import void datatype 
import Data.Aeson (FromJSON, ToJSON) -- import two functions from the library
import Data.Text qualified as T -- import everything from data.text and name them as T 
import GHC.Generics (Generic)
import Ledger (Ada, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (ScriptContext, scriptContextTxInfo),
               valuePaidTo)
import Ledger.Ada qualified as Ada -- currency
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, Promise, collectFromScript, endpoint, logInfo, selectList,
                        submitTxConstraints, submitTxConstraintsSpending, type (.\/), utxosAt)
import PlutusTx qualified
import PlutusTx.Prelude (Bool, Semigroup ((<>)), ($), (&&), (-), (.), (>=))
import Prelude qualified as Haskell
import Schema (ToSchema)
import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)

-- 2. creating a new user defined type to be passed on validator
data SplitData =
    SplitData -- declaring a record 
        { recipient1 :: PaymentPubKeyHash -- ^ First recipient of the funds
        , recipient2 :: PaymentPubKeyHash -- ^ Second recipient of the funds
        , amount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Haskell.Show, Generic) -- automatically create an instance from these typeclasses

-- For a 'real' application use 'makeIsDataIndexed' to ensure the output is stable over time
PlutusTx.unstableMakeIsData ''SplitData
PlutusTx.makeLift ''SplitData

-- 3. defining the validator script 
d -> r -> ValidatorCtx -> Bool

validateSplit :: SplitData -> () -> ScriptContext -> Bool -- () 
validateSplit SplitData{recipient1, recipient2, amount} _ ScriptContext{scriptContextTxInfo} =
    let half = Ada.divide amount 2 in
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash recipient1)) >= half &&
    Ada.fromValue (valuePaidTo scriptContextTxInfo (unPaymentPubKeyHash recipient2)) >= (amount - half)

data Split
instance Scripts.ValidatorTypes Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

splitValidator :: Scripts.TypedValidator Split
splitValidator = Scripts.mkTypedValidator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @SplitData @()

-- 4. asking for input 
data LockArgs =
        LockArgs
            { recipient1Wallet :: Wallet
            , recipient2Wallet :: Wallet
            , totalAda         :: Ada
            }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

lock :: Promise () SplitSchema T.Text ()
lock = endpoint @"lock" (lockFunds . mkSplitData)

unlock :: Promise () SplitSchema T.Text ()
unlock = endpoint @"unlock" (unlockFunds . mkSplitData)

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs{recipient1Wallet, recipient2Wallet, totalAda} =
    SplitData
        { recipient1 = mockWalletPaymentPubKeyHash recipient1Wallet
        , recipient2 = mockWalletPaymentPubKeyHash recipient2Wallet
        , amount = totalAda
        }

-- 5. locking the funds 
lockFunds :: SplitData -> Contract () SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> Haskell.show amount
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints splitValidator tx

tx = Constraints.mustPayToTheScript s (Ada.toValue amount)

-- 6. unlocking the funds
unlockFunds :: SplitData -> Contract () SplitSchema T.Text ()
unlockFunds SplitData{recipient1, recipient2, amount} = do
    let contractAddress = Scripts.validatorAddress splitValidator
    utxos <- utxosAt contractAddress
    let half = Ada.divide amount 2
        tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient1 (Ada.toValue half)
            <> Constraints.mustPayToPubKey recipient2 (Ada.toValue $ amount - half)
    void $ submitTxConstraintsSpending splitValidator utxos tx



