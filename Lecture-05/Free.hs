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

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
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
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

------------------------------------------------------------------------------
-- on chain

{-# INLINABLE mkPolicy #-}
-- could parameterize the policy (like can be done for validators); not done here
-- no datum
--      redeemer;       context
--          v              v
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True -- policy always allows minting/burning; no questions asked

-- compile policy to Plutus script
policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript
       --                      converted to untyped version
       --                              v
       $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
            -- convert to hash wrapped in CurrencySymbol
            --    v
curSymbol = scriptCurrencySymbol policy

------------------------------------------------------------------------------
-- off chain

data MintParams = MintParams
    { mpTokenName :: !TokenName -- what is being minted
                                -- only name needed since symbol is given by script
    , mpAmount    :: !Integer   -- how much is being minted (if positive)
                                --                or burned (if negative)
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
                  -- create Value to be minted or burned
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
                  -- TX must contain the actual minting policy
        lookups = Constraints.mintingPolicy policy
                  -- must mint the given value
                  -- just mentioning value, not the redeemer,
                  -- because case where redeemer is () is common
        tx      = Constraints.mustMintValue val
    --          will find input in wallet to cover fees
    --          will transfer minted value to wallet if positive
    --          will find sufficient tokens in wallet to be burned if negative
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

type FreeSchema = Endpoint "mint" MintParams

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

------------------------------------------------------------------------------
-- for playground
mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []
-- end boilerplate for playgroung

------------------------------------------------------------------------------
-- test with emulator trace

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC" -- TokenName
    -- start wallets 1 and 2
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    -- mint 555 new tokens into wallet 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    -- mint 44 new tokens into wallet 2
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    -- burn 555 tokens from wallet 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1
