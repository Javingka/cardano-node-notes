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

module Batch45Minting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)       --Nsb import
import           Data.Map             as Map                  
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      as Contract
import           Plutus.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (mint, singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE nftPolicy #-}
nftPolicy :: TcOutRef -> TokenName -> () -> ScriptContext -> Bool
nftPolicy oref tname () sContext =  traceIfFalse "UTxO not consumed" hasUTxO        &&
                                    traceIfFalse "Trying to mint the wrong amount" checkMintedAmount
    where
        info :: TxInfo
        info = ScriptContextTxInfo sContext

        hasUTxO :: Bool
        hasUTxO = (\utxo -> ixInInfoOutRef utxo == oref) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of 
            [(_, tname', amount)] -> tname' == tname && amount == 1

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tname = mkMintingPolicyScrtipt $
    $$(PlutusTx.compile [|| \oref' tname' -> Script.wrapMintingPolicy $ nftPolicy oref' tname' ||])
    `PlutusTx.apply`


--OFF-CHAIN
data NFTParams = NFTParams
    {npTokenName    :: !TokenName
    ,npAddress      :: !Address
    } deriving (Generic, ToJSON, FromJSON, Show)

type

mint :: NFTParams -> Contract w NFTSchema Text ()
mint nparams = do
    utxos <- utxosAt $ npAddress nparams
    case Map.keys utxos of
        []      -> Contract.logError @String "No UTxOfound!"
        oref:_  -> do