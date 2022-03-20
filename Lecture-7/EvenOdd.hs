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

{-
EvenOdd
- if sum of the two choices made by the players is
- even : then 1st player wins
- odd  : then 2nd player wins
-}

module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

------------------------------------------------------------------------------
-- on chain

-- parameter to contract
data Game = Game
    { gFirst          :: !PaymentPubKeyHash -- e.g., Alice.
    , gSecond         :: !PaymentPubKeyHash -- e.g., Bob.

    , gStake          :: !Integer           -- Amount of lovelace used as stake in the game by each player.

    , gPlayDeadline   :: !POSIXTime         -- 2nd player must move before this time,
                                            -- otherwise 1st player can get its stake back.

    , gRevealDeadline :: !POSIXTime         -- After 2nd player makes move, 1st player must reveal their move
                                            -- before this time, otherwise 2nd player "wins".

    , gToken          :: !AssetClass        -- of NFT that identifies the UTxO that represents state of game.
                                            -- Sometimes called a "STATE TOKEN".
                                            -- See below.
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

{-
IMPORTANT TECHNIQUE

gToken :: !AssetClass

Program has STATE, here game state (see Lecture-y.org).

Ethereum contracts have mutable state, so can just modify state in place.

Cardano : everything immutable.
Can NOT change an existing UTxO.
UTxOs are either
- unspents, or
- get spent by a TX
  - and new UTxOs get created

Instead of changing state in place, create new state (like in Haskell),
e.g., create new UTxO that contains extended/changed datum.

Use an NFT to "link" UTxOs that represent changing state.
(Note: week01 Auction does this, but was not highlighted.)

Put NFT into UTxO.
(Note: stake in game is also in state.)

1st player creates TX that does move and adds NFT to output UTxO.

2nd player creates TX that
- consumes above UTxO
- creates need UTxO that contains the *same* NFT

To find current state of game, look for UTxO at the address given by the validator script
for the game containing this NFT.

Besides linking UTxOs, the NFT is important because on Cardano,
validation only happens when a UTxO is spent, but NOT when you
produce an output sitting at the script address.

In case of this game, that means anybody could crate a UTxO
at the same address with with exactly the same datum as the 1st player.

NFT, by definition, is unique and only exists once.
Anyone can can produce UTxO that contains same datum,
but only one of the UTxOs can contain the NFT as part of its value,
because the NFT only exists once.
-}

PlutusTx.makeLift ''Game

-- the moves that a player can make
data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

-- Plutus.Eq
instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

--                         hash submitted
--                         by 1st player      2nd player move
-- contract state                 v                 v
data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice)
    deriving Show

-- Plutus.Eq
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

-- state transitions
data GameRedeemer
  = Play GameChoice          -- 2nd player move
  | Reveal BuiltinByteString -- 1st player reveals after 2nd player move
                             -- BuiltinByteString is the nonce
  | ClaimFirst               -- 2nd player does NOT move, 1st player gets their stake back
  | ClaimSecond              -- 1st player does NOT reveal, so 2nd players gets all stake (wins)
  deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: Maybe Datum -> Maybe GameDatum
gameDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkGameValidator #-}
--                              not possible to use string literals
--                              to get byte strings so these contain
--           1st param          literals for zero and one
--                 v              v                    v
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    -- game identitied by state token 'gToken'
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        -- 1st player has previusly moved, the 2nd player is MOVING, chosing 'c'
        (GameDatum bs Nothing, Play c) ->
            -- 1 check move is made by 2nd player
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            -- 2 check that 1st player has put down stake
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            -- 3 check that 2nd player adds stake in this TX
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            -- 4 output datum must be same hash as before, but now containing 2nd player's choice
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            -- 5 move must happen before first deadline
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            -- 6 the NFT must be passed onto the new UTxO
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        -- both players have previously moved, 1st player wins, so reveals to collect stake
        (GameDatum bs (Just c), Reveal nonce) ->
            -- check move is made by 1st player
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            -- check 1st player is telling the truth
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            -- must reveal before reveal deadline
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            -- input must have stake of both players
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            -- game over so NFT goes to 1st player
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- 2nd player has not previously moved, 1st player sees that deadline past, so reclaims their stake
        (GameDatum _ Nothing, ClaimFirst) ->
            -- check claim is made by 1st player
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            -- check the deadline has past
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            -- 1st player should have provided stake
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            -- game over so NFT goes to 1st player
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- both players have previously moved, the 1st player sees they have lost so remains quiet,
        -- the 2nd player sees reveal deadline has past, so claims game stake (wins).
        (GameDatum _ (Just _), ClaimSecond) ->
            -- check claim is made by 2nd player
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            -- check the deadline has past
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            -- both players should have provided stake
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            -- game over so NFT goes to 1st player
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- any other transitions are not legal
        _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    -- During state transitions, consume current game UTxO
    -- and produce a new UTxO at the same script address
    -- with an updated datum (and possibly updated value).
    -- getContinuingOutputs :: [TxOut]
    --    the outputs that sit at script address being validated
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one game output"

    -- the datum MUST be included (not just its hash)
    outputDatum :: GameDatum
    outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    -- 1st player wants to prove it has won by revealing its nonce and proving
    -- the hash submitted in beginning fits this nonce
    --
    --              hash                  nonce             the move that both players made
    --                                                      really the 2nd player choice
    --                                                      but 'checkNonce' only called
    --                                                      when they are the same
    --               v                      v                   v
    checkNonce :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        cFirst :: BuiltinByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    -- What happens to NFT when the game is over and there is no longer a UTxO at the game address?
    -- Implemented to give NFT to 1st player (who minted it).
    -- IMPORTANT discussion at 19:46 about payment part and staking part.
    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1

-- bundle info about datum and redeemer types
data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

-- compile the validator
typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

------------------------------------------------------------------------------
-- off chain

-- Find the UTxO representing the current game state,
-- e.g., that one that has the NFT.
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, GameDatum))
findGameOutput game = do
    utxos <- utxosAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
                     -- the datum for this UTxO
        dat       <- gameDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        return (oref, o, dat)
  where
    -- check that output contains NFT
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (gToken game) == 1

-- wait until the POSIXTime has past AND are in the NEXT SLOT
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2

-- two off-chain contracts for the two players

-- 1st player invokes contract/starts game
data FirstParams = FirstParams
    { fpSecond         :: !PaymentPubKeyHash -- 1st player chooses who gets to be 2nd player
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString -- 1st player's hashed move/choice
    , fpCurrency       :: !CurrencySymbol    -- NFT
    , fpTokenName      :: !TokenName         -- NFT
    , fpChoice         :: !GameChoice        -- 1st player's        move/choice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        -- 1st player's stake and the NFT
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        -- 1st player's move/choice
        c    = fpChoice fp
        -- move/choice hidden in hashed nonce
        bs   = sha2_256 $ fpNonce fp `appendByteString` if c == Zero then bsZero else bsOne
        -- produce an output at script address with datum that contains the hash AND the 'v' (state/NFT)
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    -- give time for 2nd player to move
    waitUntilTimeHasPassed $ fpPlayDeadline fp

    -- get current game state
    m   <- findGameOutput game
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            --   2nd player has not moved (and deadline has past)
            --             v
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                -- 1st player gets stake back
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "reclaimed stake"

            -- 2nd player did move and move was the same as the 1st player's move
            --            v               v
            GameDatum _ (Just c') | c' == c -> do

                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)

                              -- 1st player must Reveal to prove they won (and to get game stake)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              -- must submit before 'reveal' deadline
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "victory"

            -- 2nd player won so don't do anything
            _ -> logInfo @String "second player played and won"

-- 2nd player plays
data SecondParams = SecondParams
    { spFirst          :: !PaymentPubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice -- 2nd player choice/move
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game
    case m of
        -- 1st player has moved
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            let token   = assetClassValue (gToken game) 1 -- NFT
                          -- add our stake into existing stake (and existing state token) to 'v'alue
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp

                          -- the UTxO being consumed
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          -- because consuming one
                          Constraints.otherScript (gameValidator game)                                        <>
                          -- because producing one
                          Constraints.typedValidatorLookups (typedGameValidator game)

                          -- spend existing UTxO with our choice/move
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>
                          -- create new UTxO with updated Datum
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>
                          -- do before deadline
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = getCardanoTxId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            -- give 1st player time to move
            waitUntilTimeHasPassed $ spRevealDeadline sp

            m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                Nothing             -> logInfo @String "first player won"
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"

                                   -- the UTxO being consumed
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   -- the validator
                                   Constraints.otherScript (gameValidator game)

                                   -- spend the UTxO
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                   -- before the deadline
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   -- give NFT back to 1st player (required to add some min Ada to it)
                                   -- note: unfair: 2nd player has to pay min Ada if they win (coud make it fair, but not done)
                                   Constraints.mustPayToPubKey (spFirst sp) (token <> adaValueOf (getAda minAdaTxOut))
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

-- assemble two contracts into a single contract
endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame
