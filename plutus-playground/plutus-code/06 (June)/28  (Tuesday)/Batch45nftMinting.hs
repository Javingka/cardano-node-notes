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
import           Wallet.Emulator.