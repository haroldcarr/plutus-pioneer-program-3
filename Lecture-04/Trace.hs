{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

-- copy from Week03
import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

-- This does the steps that were done in the playground in Week03.
myTrace :: EmulatorTrace ()
myTrace = do
    -- activate the two wallets
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints

    -- Wallet 1 Give
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }

    -- wait until deadline as past
    void $ waitUntilSlot 20

    -- Wallet 2 Grab
    callEndpoint @"grab" h2 ()

    s <- waitNSlots 2

    Extras.logInfo $ "reached " ++ show s
