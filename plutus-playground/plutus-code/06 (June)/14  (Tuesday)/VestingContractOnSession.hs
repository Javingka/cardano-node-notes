{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module VestingContract where

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

--ON CHAIN CODE

data VestingDatum = VestingDatum -- is gonna put on UTXOs Datum with information of the beneficiary and the data deadline
    {
        beneficiary :: PaymentPubKeyHash,
        deadline :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE vestingValidator #-}
vestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
vestingValidator datum () sContext = traceIfFalse "Beneficiary's signature missing" signedByBeneficiary &&
                                     traceIfFalse "Deadline not reached" deadlineReached -- <- here the conditions aimed to be meet in order to validate transaction
    where
        info :: TxInfo
        info = scriptContectTxInfo sContext 

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $beneficiary datum

        deadlineReached :: Bool
        deadlineReached = contains(from $ deadline datum) txInfoValidRange sContext
