{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

------------------------------------------------------------------------------
-- on chain

-- This script always passes.
-- An output sitting at this script address means that
-- - arbitrary TXs can use that output as input
-- - does not matter what datum or redeemer or TX are used (since ignored)
-- If anyone sends funds to this script address
-- then anyone else can consume that output.
-- So sending funds to this address is a GIFT - anyone can take those funds.
--
--    from output being consumed
--              |      from input that is consumer
--                               |         tx with all inputs/outputs
--                                                          () if success; throws error otherwise
--              v                v             v            v
--             datum          redeemer       context        result
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()
{-# INLINABLE mkValidator #-}

-- The "real" validator : compile above to Plutus core.
-- All code transitively referenced must be INLINABLE.
validator :: Validator
validator  = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash  = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress  = scriptAddress validator

------------------------------------------------------------------------------
-- off chain

-- endpoints are ways for the user to trigger TX, enter data, give input

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript
               valHash                      -- above script
               (Datum $ Builtins.mkI 0)     -- will be ignored, but need to satisfy compiler
               (Ada.lovelaceValueOf amount) -- convert into lovelace
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab  = do
    -- get all UTxO at script address
    utxos <- utxosAt scrAddress
    let
        -- get references to those utxos
        orefs   = fst <$> Map.toList utxos

        -- constructor TX at all utxos (i.e., at script address)
        tx     :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]

        -- tell wallet how to construct the TX
        lookups = Constraints.unspentOutputs utxos  -- where to find the UTxOs
               <> Constraints.otherScript validator -- to consume a UTxO sitting at script address
                                                    -- must provide the script

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

type GiftSchema =
            Endpoint "give" Integer -- how much to give
        .\/ Endpoint "grab" ()      -- grab all of it

-- 'select' : make 'give' and 'grab' available
-- '>>'     : make them available again
endpoints :: Contract () GiftSchema Text ()
endpoints  = awaitPromise (give' `select` grab') >> endpoints
  where
    -- waits for user to provide amount
    give' = endpoint @"give" give
    grab' = endpoint @"grab" (const grab)

-- boilerplate

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
