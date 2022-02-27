{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework1 where

import           Control.Monad              hiding (fmap)
import           Data.Default               (Default (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import           Ledger.Constraints         as Constraints
import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Playground.Contract
import           Prelude                    (Semigroup (..), Show (..), String)
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

------------------------------------------------------------------------------
-- on chain

{-# INLINABLE mkPolicy #-}
-- This policy only allows minting/burning of tokens if
-- the owner of specified PaymentPubKeyHash has signed the transaction, and
-- the specified deadline has not passed.
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =
  traceIfFalse "ON-CHAIN: signature failure"
               (txSignedBy (scriptContextTxInfo ctx) (unPaymentPubKeyHash pkh))
  &&
  traceIfFalse "ON-CHAIN: past deadline"
               (contains (to deadline) (txInfoValidRange (scriptContextTxInfo ctx)))

policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \p d -> Scripts.wrapMintingPolicy $ mkPolicy p d ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol (policy pkh deadline)

------------------------------------------------------------------------------
-- off chain

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpDeadline  :: !POSIXTime
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    now <- Contract.currentTime
    let deadline = mpDeadline mp
    if now > deadline -- SET THIS TO 'False' to ensure policy checks deadline correctly
        then Contract.logError @String "OFF-CHAIN: deadline passed"
        else do
            let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                lookups = Constraints.mintingPolicy (policy pkh deadline)
                tx      = Constraints.mustMintValue val
                       <> Constraints.mustValidateIn (to $ now + 60000)
            ledgerTx   <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String (printf "OFF-CHAIN: forged %s" (show val))

type SignedSchema = Endpoint "mint" MintParams

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

------------------------------------------------------------------------------

mkSchemaDefinitions ''SignedSchema
mkKnownCurrencies []

------------------------------------------------------------------------------

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn       = "ABC"
        deadline = slotToBeginPOSIXTime def 100
    h <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 110
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1
