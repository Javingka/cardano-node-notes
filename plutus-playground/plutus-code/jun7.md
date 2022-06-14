(8:44) Rerview the sources where to find Documentation regarding the Types

Link where you can review the types:
https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/Builtins/Internal.hs

or 

https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Scripts.html
wich can be compiled in your own Plutus playground

(8:52) good Redeemer review. Roberto just take the code and compiled in order to check it out.
 
 (8:55) Testing the code. which one? the Redeemer? maybe


 (9) OurTypedRedeemer.hs start from 'scratch'
 The main difference between this from beodre
 uis the Typed Libraries.
 from: 
import qualified Ledger.Scripts      as Scripts
to

import qualified Ledger.Typed.Scripts      as Scripts

 Most of the functions havbe the same name (at least the ones we will be using)

(9:08) when using type validator you need to do a Map from high level to low level. 
(9:05 - 9:15) mounting the on chain code for Typed Redeemer

(9:16) Review on `Scripts.validatorTypes`

(9:18) loak at `wrapValidator`


(9:20) paste off-chain code, no explanations


(9:25) Why change fees according the typed or not redeemer?
Basically when you use more RAM and CPU power, including for example a wrapping function, on the Network Nodes, they will charge for the service.

(9:32) So, having higher fees in which scenario is it convenient to use typed redeemers? 
(9:35) respond as Redability by the use of sintactic sugar

(9:46) custom redemeer data type
(10:10) Roberto compile the custom data type example
