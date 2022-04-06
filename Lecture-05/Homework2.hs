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

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

------------------------------------------------------------------------------

tn :: TokenName
tn  = TokenName emptyByteString

------------------------------------------------------------------------------
-- on chain

{-# INLINABLE mkPolicy #-}
-- one parameter
--   reference to TX output - a UTxO
--             v
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "ON-CHAIN: UTxO not consumed"   hasUTxO
                    && traceIfFalse "ON-CHAIN: wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    --      do any match the given UTxO?             inputs to TX
    --                         v                         v
    hasUTxO = any (\i -> txInInfoOutRef i == oref) (txInfoInputs info)

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        -- expect on ONE coin
        --              with correct name and amount
        --           v            v            v
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' -> Scripts.wrapMintingPolicy $ mkPolicy oref' ||])
    `PlutusTx.applyCode`   -- how to handle multiple parameters
    PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol  = scriptCurrencySymbol . policy

------------------------------------------------------------------------------
-- off chain

mint :: Address -> Contract w NFTSchema Text ()
mint address = do
    -- get all map of TxOutRef to outputs at user's address
    utxos <- utxosAt address
    -- gets the keys (i.e., TxOutRef)
    case Map.keys utxos of
        []       -> Contract.logError @String "OFF-CHAIN: no utxo found"
        -- just take the first one
        oref : _ -> do
            let val     = Value.singleton (curSymbol oref) tn 1
                -- must include minting policy script in TX
                --                                   v
                lookups = Constraints.mintingPolicy (policy oref)
                -- Need UTxOs to construct TX.
                -- Do not need all UTxOs, just the 'oref' one.
                -- But OK to give all UTxOs, as long as 'oref' is included.
                       <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val
                --        specify it must spend oref
                       <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx   <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "OFF-CHAIN: forged %s" (show val)

type NFTSchema = Endpoint "mint" Address

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

------------------------------------------------------------------------------

test :: IO ()
test = runEmulatorTraceIO $ do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ mockWalletAddress w1
    callEndpoint @"mint" h2 $ mockWalletAddress w2
    void $ Emulator.waitNSlots 1
