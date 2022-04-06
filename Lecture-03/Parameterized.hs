{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Parameterized where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

data VestingParam = VestingParam
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

-- enable compiling to Plutus
PlutusTx.makeLift ''VestingParam

{-# INLINABLE mkValidator #-}
--             parameter        now datum NOT used
               v                v
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
  traceIfFalse "deadline not reached" deadlineReached
   where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool                                              -- v
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash $ beneficiary p

    deadlineReached :: Bool                  -- v
    deadlineReached = contains (from $ deadline p) $ txInfoValidRange txInfo

-- boilerlate

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = () -- unit
    type instance RedeemerType Vesting = ()

typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    -- ADD PARAMETER
    --                                          this part is runtime
    -- this part is compiletime               v ------------------------------------ v
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

--           v
validator :: VestingParam -> Validator
                                 -- v (i.e., "point-free")
validator = Scripts.validatorScript . typedValidator

--         v
valHash :: VestingParam -> Ledger.ValidatorHash
                             -- v
valHash = Scripts.validatorHash . typedValidator

--            v
scrAddress :: VestingParam -> Ledger.Address
                        -- v
scrAddress = scriptAddress . validator

------------------------------------------------------------------------------
-- off chain

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" POSIXTime -- need to know deadline
                           -- ^

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let p     = VestingParam { beneficiary = gpBeneficiary gp
                             , deadline    = gpDeadline    gp }
        tx    = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
        --                                          v
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

--                                         v
grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab d = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    -- check via input (instead of datum)
    if now < d
        then logInfo @String $ "too early"
        else do
            let p = VestingParam { beneficiary = pkh
                                 , deadline    = d }
            utxos <- utxosAt $ scrAddress p
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos
                                                                  -- v
                               <> Constraints.otherScript (validator p)
                        tx :: TxConstraints Void Void
                        tx      = mconcat [ Constraints.mustSpendScriptOutput oref unitRedeemer
                                          | oref <- orefs ]
                               <> Constraints.mustValidateIn (from now)
                    ledgerTx   <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
