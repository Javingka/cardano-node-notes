{-# LANGUAGE DataKinds           #-}
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

module Batch45SignedMinting where

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

--ON-CHAIN

{-# INLINABLE signedMintingPolicy  #-}
-- the second parameter is the redeemer
signedMintingPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
signedMintingPolicy ppkh _ sContext = traceIfFalse "Not the right signature" isTheRightSignature
    where
        info :: TxInfo
        info = scriptContextTxInfo sContext

        isTheRightSignature :: Bool
        isTheRightSignature = txSignedBy info $ unPaymentPubKeyHash ppkh -- txSignedBy  :: TxInfo -> PubKeyHash -> Bool 

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $
            $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy signedMintingPolicy  ||])
            `PlutusTx.applyCode` -- used to 'join' the parameters with the function
            Plutus.liftCode pkh

policyId :: PaymentPubKeyHash -> CurrencySymbol
policyId  = scriptCurrencySymbol . policy

