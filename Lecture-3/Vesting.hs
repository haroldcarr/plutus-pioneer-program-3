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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

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

data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash  -- who receives that ADA
    , deadline    :: POSIXTime          -- can receive after this time
    } deriving Show

-- turn custom type in `BuiltInData`
-- note use "stable" version for production
PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
--                  no additional info needed to redeem
--                             v
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator datum () ctx =
  -- to unlock
  -- must be signed by beneficiary
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
  -- and the deadline reached
  traceIfFalse "deadline not reached" deadlineReached
   where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
                       -- txSignedBy :: TxInfo -> PubKeyHash -> Bool
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash $ beneficiary datum

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline datum) $ txInfoValidRange txInfo

-- boilerlate

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

------------------------------------------------------------------------------
-- off chain

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash -- identity of receiver
    , gpDeadline    :: !POSIXTime         -- time after which OK to grab
    , gpAmount      :: !Integer           -- how much is given
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- endpoints
type VestingSchema =
            Endpoint "give" GiveParams -- for entity setting up contract to give ADA
        .\/ Endpoint "grab" ()         -- for entity collecting the ADA

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let datum = VestingDatum { beneficiary = gpBeneficiary gp
                             , deadline    = gpDeadline    gp }
        tx    = Constraints.mustPayToTheScript datum $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    -- lookup all UTxOs at the script address (might be empty)
    -- select the ones for "me" whoses deadlines have been reached (might be empty)
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            -- create a TX that tries to collect ALL found
            -- note : there is a size limit to number of TX, so ALL might be too much
            let -- get tx orefs
                orefs   = fst <$> Map.toList utxos
                -- contains the UTxOs and the validator of the script
                lookups = Constraints.unspentOutputs utxos
                       <> Constraints.otherScript validator
                -- want to spend the script output of each reference
                tx :: TxConstraints Void Void
                tx      = mconcat [ Constraints.mustSpendScriptOutput oref unitRedeemer
                                  | oref <- orefs ]
                       -- ***** TIME *****
                       -- specify TX is later than the current time
                       <> Constraints.mustValidateIn (from now)
            ledgerTx   <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    --                                              output of UTxO
    --                                                    v
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o =
      -- This ensure that the TX is only submitted if the UTxO can be "grabbed".
      -- This means that the on-chain validator is not really exercised,
      -- since only valid TXs are submitted.

      -- convert datum to custom type
      case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            -- once in custom type, check condition
            Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
