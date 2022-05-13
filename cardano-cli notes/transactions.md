# call protocol apram,s
cardano-cli query protocol-parameters \
> $TESTNET \
> --out-file protocolparams.json


cardano-cli transaction build-raw --shelley-era --tx-in debe97bf7a29f4affd0f910b29af30912d720050748e5e82ce1ef52d5bdaebd5#0 --tx-out $(cat payment12.addr)+500000000 --tx-out $(cat payment.addr)+0 --invalid-hereafter 0 --fee 0 --out-file tx.draft

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction view --tx-body-file tx.draft

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli text-view decode-cbor --in-file tx.draft --out-file tx.draft2

# calculate fee
javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction calculate-min-fee \
> --tx-body-file tx.draft \
> --tx-in-count 1 \
> --tx-out-count 2 \
> --witness-count 1 \
> $TESTNET \
> --protocol-params-file protocolparams.json 


# fee calculated:
176897

#slot when deploy transaction:

transaction detail
100,000,000 - 500,000,000 - 176,897 = 499,823,103

calculate at what slot
56263178 <- actual
56263578 <- the new

# actual transaction
javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction build-raw --shelley-era --tx-in debe97bf7a29f4affd0f910b29af30912d720050748e5e82ce1ef52d5bdaebd5#0 --tx-out $(cat payment12.addr)+500000000 --tx-out $(cat payment.addr)+499823103 --invalid-hereafter 56263578 --fee 0 --out-file tx.raw

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file payment.skey $TESTNET --out-file tx.signed

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction submit --tx-file tx.signed $TESTNET

Previous command gives error:
Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (MissingVKeyWitnessesUTXOW (WitHashes (fromList [KeyHash "e432b9bf3538a07fe50a01e2f576ae75eeb64fd82dcf77eb4c73297f"])))),UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (ValueNotConservedUTxO (Value 1000000000 (fromList [])) (Value 999823103 (fromList []))))),UtxowFailure (WrappedShelleyEraFailure (UtxoFailure (FeeTooSmallUTxO (Coin 167921) (Coin 0))))])

-- so again....

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction build-raw --alonzo-era --tx-in debe97bf7a29f4affd0f910b29af30912d720050748e5e82ce1ef52d5bdaebd5#0 --tx-out $(cat payment12.addr)+500000000 --tx-out $(cat payment.addr)+0 --invalid-hereafter 0 --fee 0 --out-file tx.draft

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction build-raw --alonzo-era --tx-in debe97bf7a29f4affd0f910b29af30912d720050748e5e82ce1ef52d5bdaebd5#0 --tx-out $(cat payment12.addr)+25000000 --tx-out $(cat payment.addr)+0 --invalid-hereafter 0 --fee 0 --out-file tx.draft

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction calculate-min-fee --tx-body-file tx.draft --tx-in-count 1 --tx-out-count 2 --witness-count 1 $TESTNET --protocol-params-file protocolparams.json 

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction build-raw --alonzo-era --tx-in debe97bf7a29f4affd0f910b29af30912d720050748e5e82ce1ef52d5bdaebd5#0 --tx-out $(cat payment12.addr)+500000000 --tx-out $(cat payment.addr)+499822927 --invalid-hereafter 56264300 --fee 177073 --out-file tx.raw

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction sign --tx-body-file tx.raw --signing-key-file payment.skey $TESTNET --out-file tx.signed

javingka@javingka-B450-AORUS-ELITE:~/test01$ cardano-cli transaction submit --tx-file tx.signed $TESTNET

error again

Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraAlonzo (ApplyTxError [UtxowFailure (WrappedShelleyEraFailure (MissingVKeyWitnessesUTXOW (WitHashes (fromList [KeyHash "e432b9bf3538a07fe50a01e2f576ae75eeb64fd82dcf77eb4c73297f"]))))])

-- again
-- problems with addresses so, lets create the one missing





