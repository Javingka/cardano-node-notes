{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module GuessingGame where

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
import           Prelude              qualified as Haskell
import           Text.Printf          (printf)


--ON CHAIN

data HashedString = HS BuildinByteString
data ClearString = CS BuildinByteString

-- Call the following to create instances of the data types
PlutusTx.unstableMakeIsData ''HashedString
PlutusTx.unstableMakeIsData ''ClearString

data Game 
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

{-# INLINABLE validateGuesss #-}
validateGuesss :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuesss hs cs _ = isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HS theSecret) (CS theGuess) =  secret == sha2_256 guess

gameInstance :: Scripts.TypedValidator Game 
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuesss ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Script.wrapValidator @HashedString @ClearString


gameValidator :: Validator
gameValidator = Script.validatorScript gameInstance

gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- OFF CHAIN

data BetParams = BP 
    { 
        theSecret :: Haskell.String
    ,   theAmount :: Value
    } deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data GuessParam = GP 
    {
        guessWord :: Haskell.String
    } deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

type GameSchema = 
        Endpoint "theBet" BetParams 
    .\/ Endpoint "theGuess" GuessParam

theBet :: BetParams -> Contract () GameSchema Text ()
theBet (BP secret bet) = do
    let tx = Constraints.mustPayToTheScript ( hashString secret) bet
    ledgerTx <- submitTxConstraints gameInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    ...
    ...

theGuess :: GuessParam -> Contract () GameSchema Text ()
theGuess (GP guess) = do
    utxos <- utxosAt gameAddress
    case Map.toList utxos of
        []                  -> logInfo @Haskell.String $ printf "No UTxOs on the Contract!"
        (oref,a):utxos      -> do
            let redemeer = clearString guess
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
                          Constraints.otherScript gameValidator
            let tx = Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toBuiltinData redeemer)
            ledger <- submitTxConstraintsWith @Void lookups tx


hashString :: Haskell.String -> HashedString
hashString = HS . sha2_256 . toBuiltinData . C.pack

clearString :: Haskell.String -> ClearString
clearString = CS . toBuiltin . C.pack