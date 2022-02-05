{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

------------------------------------------------------------------------------

-- no inter-contract communication    no endpoints    error msgs    no return value
--                      v             v               v             v
myContract1 :: Contract ()            Empty           Text          ()
myContract1 = do
    void $ Contract.throwError "BOOM!" -- *** run with/out this commented out
    Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1 -- active contract

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

------------------------------------------------------------------------------

-- runs myContract1 and catch the exception thrown by myContract1
myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

------------------------------------------------------------------------------

-- endpoints that can be invoked from outside and put data into the contract from the outside
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3 :: Contract () MySchema Text ()
myContract3 = do

    -- turns promise into a contract
    awaitPromise $
      -- endpoint :: (..., EndpointValue a, ...)
      --          => (a -> Contract w s e b) -> Promise w s e b
      -- block until endpoint "foo" called from outside with the Int required by "foo"
      endpoint @"foo" Contract.logInfo

    -- block until "bar" called
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    -- start contract
    h <- activateContractWallet (knownWallet 1) myContract3

    -- call "foo" endpoint of contract handle (h) with the required type Int
    callEndpoint @"foo" h 42

    --- ditto for "bar"
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

------------------------------------------------------------------------------

-- specifies first type parameter 'w' as [Int] (must be a monoid)
--                      v
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1] -- write to 'w' (i.e., tell the outside what is happening)
    void $ Contract.waitNSlots 10
    tell [2] -- write to 'w'
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    -- start the contract
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h      -- see the "tell"
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
