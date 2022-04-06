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
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
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
-- on chain

{-# INLINABLE mkPolicy #-}
-- two parameters
--   reference to TX output - a UTxO
--                       name of NFT
--             v             v
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO
                       && traceIfFalse "wrong amount minted" checkMintedAmount
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

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`   -- how to handle multiple parameters
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol (policy oref tn)

------------------------------------------------------------------------------
-- off chain

data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address -- user's address
    } deriving (Generic, FromJSON, ToJSON, Show)

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    -- get all map of TxOutRef to outputs at user's address
    utxos <- utxosAt $ npAddress np
    -- gets the keys (i.e., TxOutRef)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        -- just take the first one
        oref : _ -> do
            let tn      = npToken np
            let val     = Value.singleton (curSymbol oref tn) tn 1
                -- must include minting policy script in TX
                --                                   v
                lookups = Constraints.mintingPolicy (policy oref tn)
                -- Need UTxOs to construct TX.
                -- Do not need all UTxOs, just the 'oref' one.
                -- But OK to give all UTxOs, as long as 'oref' is included.
                       <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val
                --        specify it must spend oref
                       <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx   <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

type NFTSchema = Endpoint "mint" NFTParams

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

------------------------------------------------------------------------------

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
